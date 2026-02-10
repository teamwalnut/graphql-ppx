open Graphql_compiler
open Result_structure
open Generator_utils
open Extract_type_definitions
open Output_utils
open Ppxlib
open Uncurried_utils

exception Cant_find_fragment_type_with_loc of Source_pos.ast_location * string

type operation_type = Graphql_ast.operation_type
type operation_options = { has_required_variables : bool }
type definition = Fragment | Operation of operation_type * operation_options

module StringSet = Set.Make (String)

module VariableFinderImpl = struct
  type t = StringSet.t ref

  let make_self _ = ref StringSet.empty

  include Traversal_utils.AbstractVisitor

  let enter_variable_value self _ v =
    self := StringSet.add v.Source_pos.item !self

  let from_self (self : t) : StringSet.t = !self
end

let constraint_on_type ?loc exp type_name =
  Ast_helper.Exp.constraint_ ?loc exp
    (base_type_name
       (match type_name with None -> "string" | Some type_name -> type_name))

module VariableFinder = Traversal_utils.Visitor (VariableFinderImpl)

let find_variables config document =
  let ctx = Traversal_utils.make_context config document in
  VariableFinderImpl.from_self (VariableFinder.visit_document ctx document)

let pretty_print (query : string) : string =
  let indent = ref 1 in
  let str =
    query |> String.split_on_char '\n'
    |> List.map (fun l -> String.trim l)
    |> List.filter (fun l -> l <> "")
    |> List.map (fun line ->
         let prevIndent = !indent in
         String.iter
           (function
             | '{' -> indent := !indent + 1
             | '}' -> indent := !indent - 1
             | _ -> ())
           line;
         let currIndent =
           match prevIndent < !indent with
           | true -> prevIndent
           | false -> !indent
         in
         let currIndent =
           match currIndent < 1 with true -> 1 | false -> currIndent
         in
         let line = String.make (currIndent * 2) ' ' ^ line in
         line)
    |> String.concat "\n"
  in
  str ^ "\n"

let compress_parts (parts : Graphql_printer.t array) =
  let open Graphql_printer in
  parts |> Array.to_list
  |> List.fold_left
       (fun acc curr ->
         match (acc, curr) with
         | String s1 :: rest, String s2 -> String (s1 ^ s2) :: rest
         | acc, curr -> curr :: acc)
       []
  |> List.rev |> Array.of_list

let make_fragment_query ?loc f =
  Ast_helper.Exp.ident ?loc
    { Location.txt = Longident.parse (f ^ ".query"); loc = Location.none }

let emit_printed_template_query (parts : Graphql_printer.t array) config =
  let open Graphql_printer in
  let fragment_query_refs =
    match config.fragment_in_query with
    | Include ->
      parts
      |> Array.fold_left
           (fun acc -> function
             | String _ -> acc
             | FragmentNameRef _ -> acc
             | FragmentQueryRef f -> f.contents :: acc)
           []
      |> List.rev
    | Exclude -> []
  in
  let query =
    Array.fold_left
      (fun acc -> function
        | String s -> acc ^ s
        | FragmentNameRef f -> acc ^ f.contents.item
        | FragmentQueryRef _ -> acc)
      "" parts
  in
  let fragment_names =
    fragment_query_refs |> List.mapi (fun i _frag -> "frag_" ^ string_of_int i)
  in
  let fragments =
    fragment_names |> List.map (fun name -> "${" ^ name ^ "}\n")
  in
  query :: fragments |> List.fold_left (fun acc el -> acc ^ el) ""

let emit_printed_query ~config parts =
  let open Graphql_printer in
  let make_string s =
    Ast_helper.Exp.constant (Parsetree.Pconst_string (s, Location.none, None))
  in
  let join part1 part2 =
    Ast_helper.Exp.apply
      (Ast_helper.Exp.ident
         (* TODO: figure out if string concat operator also need an uncurried annotation *)
         { Location.txt = Longident.parse "^"; loc = Location.none })
      [ (Nolabel, part1); (Nolabel, part2) ]
  in
  let query =
    Array.fold_left
      (fun acc -> function
        | String s -> acc ^ s
        | FragmentNameRef f -> acc ^ f.contents.item
        | FragmentQueryRef _ -> acc)
      "" parts
  in
  let fragment_query_refs =
    match config.fragment_in_query with
    | Ppx_config.Include ->
      parts
      |> Array.fold_left
           (fun acc -> function
             | String _ -> acc
             | FragmentNameRef _ -> acc
             | FragmentQueryRef f ->
               make_fragment_query
                 ~loc:(config.map_loc f.contents.span |> Output_utils.conv_loc)
                 f.contents.item
               :: acc)
           []
      |> List.rev
    | Exclude -> []
  in
  fragment_query_refs
  |> List.fold_left (fun acc el -> join acc el) (make_string query)

let rec list_literal = function
  | [] ->
    Ast_helper.Exp.construct
      { txt = Longident.Lident "[]"; loc = Location.none }
      None
  | value :: values ->
    Ast_helper.Exp.construct
      { txt = Longident.Lident "::"; loc = Location.none }
      (Some (Ast_helper.Exp.tuple [ value; list_literal values ]))

let loc = Location.none

let rec emit_json config = function
  | `Assoc vs ->
    let pairs =
      Ast_helper.Exp.array
        (vs
        |> List.map (fun (key, value) ->
             Ast_helper.Exp.tuple
               [
                 Ast_helper.Exp.constant
                   (Pconst_string (key, Location.none, None));
                 emit_json config value;
               ]))
    in
    add_uapp
      (Ast_helper.Exp.apply
         (Ast_helper.Exp.ident
            {
              txt =
                Longident.Ldot
                  (Longident.Ldot (Longident.Lident "JSON", "Encode"), "object");
              loc = Location.none;
            })
         [ (Nolabel, [%expr Dict.fromArray [%e pairs]]) ])
  | `List ls ->
    let values = Ast_helper.Exp.array (List.map (emit_json config) ls) in
    add_uapp [%expr JSON.Encode.array [%e values]]
  | `Bool true -> [%expr JSON.Encode.bool true]
  | `Bool false -> [%expr JSON.Encode.bool false]
  | `Null -> [%expr JSON.Encode.null]
  | `String s ->
    add_uapp
      [%expr
        JSON.Encode.string
          [%e Ast_helper.Exp.constant (Pconst_string (s, Location.none, None))]]
  | `Int i ->
    add_uapp
      [%expr
        JSON.Encode.float
          [%e Ast_helper.Exp.constant (Pconst_float (string_of_int i, None))]]
  | `StringExpr parts ->
    add_uapp [%expr JSON.Encode.string [%e emit_printed_query ~config parts]]

let wrap_template_tag ?import ?location ?template_tag source =
  match (import, location, template_tag) with
  | None, Some location, _ | Some "default", Some location, _ ->
    "require(\"" ^ location ^ "\")" ^ "`\n" ^ source ^ "`"
  | Some import, Some location, _ ->
    "require(\"" ^ location ^ "\")." ^ import ^ "`\n" ^ source ^ "`"
  | _, _, Some template_tag -> template_tag ^ "`\n" ^ source ^ "`"
  | _ -> source

let wrap_structure_raw contents =
  Ast_helper.Str.extension
    ( { txt = "raw"; loc = Location.none },
      PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( Ast_helper.Exp.constant
                    (Parsetree.Pconst_string (contents, Location.none, None)),
                  [] );
            pstr_loc = Location.none;
          };
        ] )

let wrap_raw contents =
  Ast_helper.Exp.extension
    ( { txt = "raw"; loc = Location.none },
      PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( Ast_helper.Exp.constant
                    (Parsetree.Pconst_string (contents, Location.none, None)),
                  [] );
            pstr_loc = Location.none;
          };
        ] )

let make_printed_query config definition =
  let definition_loc = Graphql_ast.get_spanning_of_definition definition in
  let source = Graphql_printer.print_document config.schema [ definition ] in
  match Ppx_config.output_mode () with
  | Ppx_config.Apollo_AST ->
    Ast_serializer_apollo.serialize_document source [ definition ]
    |> emit_json config
  | Ppx_config.String -> (
    match (config.template_tag_is_function, config.template_tag) with
    | Some true, (template_tag, location, _import)
      when location <> None || template_tag <> None ->
      let source_list = source |> Array.to_list in
      Ast_helper.Exp.apply
        (Ast_helper.Exp.ident
           {
             Location.txt =
               Longident.Lident (template_tag |> Option.get_or_else "graphql");
             loc = Location.none;
           })
        [
          ( Nolabel,
            Ast_helper.Exp.array
              (emit_printed_query ~config
                 (source_list
                 |> List.filter (function
                      | Graphql_printer.FragmentQueryRef _ -> false
                      | _ -> true)
                 |> Array.of_list)
              :: (source_list
                 |> filter_map (function
                      | Graphql_printer.FragmentQueryRef _ -> Some [%expr ""]
                      | _ -> None))) );
          ( Nolabel,
            Ast_helper.Exp.array
              (source_list
              |> filter_map (function
                   | Graphql_printer.FragmentQueryRef x ->
                     Some (make_fragment_query x.contents.item)
                   | _ -> None)) );
        ]
    | template_tag_is_function, (template_tag, location, _import)
      when (location <> None || template_tag <> None)
           && template_tag_is_function <> Some true ->
      let source_list = source |> Array.to_list in
      Ast_helper.Exp.apply
        (Ast_helper.Exp.ident
           ~loc:(config.map_loc definition_loc |> Output_utils.conv_loc)
           {
             Location.txt =
               Longident.Lident (template_tag |> Option.get_or_else "graphql");
             loc = config.map_loc definition_loc |> Output_utils.conv_loc;
           })
        [
          ( Nolabel,
            Ast_helper.Exp.array
              (emit_printed_query ~config
                 (source_list
                 |> List.filter (function
                      | Graphql_printer.FragmentQueryRef _ -> false
                      | _ -> true)
                 |> Array.of_list)
              :: (source_list
                 |> filter_map (function
                      | Graphql_printer.FragmentQueryRef _ -> Some [%expr ""]
                      | _ -> None))) );
          ( Nolabel,
            Ast_helper.Exp.array
              (source_list
              |> filter_map (function
                   | Graphql_printer.FragmentQueryRef x ->
                     let fragment_query =
                       make_fragment_query
                         ~loc:
                           (config.map_loc x.contents.span
                           |> Output_utils.conv_loc)
                         x.contents.item
                     in
                     if x.allow_string then
                       (* this coerces the value from a string to the desired return type *)
                       Some
                         (Ast_helper.Exp.apply
                            (Ast_helper.Exp.ident
                               {
                                 Location.txt =
                                   Longident.Lident "graphql_allow_string";
                                 loc = Location.none;
                               })
                            [
                              ( Nolabel,
                                constraint_on_type
                                  ~loc:
                                    (config.map_loc x.contents.span
                                    |> Output_utils.conv_loc)
                                  fragment_query (Some "string") );
                            ])
                     else Some fragment_query
                   | _ -> None)) );
        ]
    | _ -> emit_printed_query ~config source)

let signature_module name signature =
  {
    psig_loc = Location.none;
    psig_desc =
      Psig_module
        {
          pmd_loc = Location.none;
          pmd_name =
            {
              txt = Some (Generator_utils.capitalize_ascii name);
              loc = Location.none;
            };
          pmd_type = Ast_helper.Mty.signature signature;
          pmd_attributes = [];
        };
  }

let wrap_module ~loc:_ ?module_type (name : string) contents =
  let loc = Location.none in
  {
    pstr_desc =
      Pstr_module
        {
          pmb_name = { txt = Some (Generator_utils.capitalize_ascii name); loc };
          pmb_expr =
            (match module_type with
            | Some module_type ->
              {
                pmod_desc =
                  Pmod_constraint
                    (Ast_helper.Mod.structure contents, module_type);
                pmod_loc = loc;
                pmod_attributes = [];
              }
            | None -> Ast_helper.Mod.structure contents);
          pmb_attributes = [];
          pmb_loc = loc;
        };
    pstr_loc = loc;
  }

let get_functor config definition =
  match config.extend with
  | Some funct -> Some funct
  | None -> (
    match definition with
    | Fragment -> Ppx_config.extend_fragment ()
    | Operation (Query, { has_required_variables = false }) -> (
      match Ppx_config.extend_query_no_required_variables () with
      | Some extension -> Some extension
      | None -> Ppx_config.extend_query ())
    | Operation (Query, _) -> Ppx_config.extend_query ()
    | Operation (Mutation, { has_required_variables = false }) -> (
      match Ppx_config.extend_mutation_no_required_variables () with
      | Some extension -> Some extension
      | None -> Ppx_config.extend_mutation ())
    | Operation (Mutation, _) -> Ppx_config.extend_mutation ()
    | Operation (Subscription, { has_required_variables = false }) -> (
      match Ppx_config.extend_subscription_no_required_variables () with
      | Some extension -> Some extension
      | None -> Ppx_config.extend_subscription ())
    | Operation (Subscription, _) -> Ppx_config.extend_subscription ())

let wrap_query_module ~loc:module_loc ~module_type definition name contents
  config =
  let loc = Location.none in
  let module_name =
    match name with Some name -> name ^ "_inner" | None -> "Inner"
  in
  let funct = get_functor config definition in
  let signature =
    match module_type with
    | { pmty_desc = Pmty_signature signature } -> signature
    | _ -> []
  in
  let inner_module =
    wrap_module ~module_type ~loc:Location.none module_name contents
  in
  let contents, module_type =
    match funct with
    | Some funct ->
      let contents =
        [
          inner_module;
          Ast_helper.Str.include_
            (Ast_helper.Incl.mk
               (Ast_helper.Mod.ident { txt = Longident.parse module_name; loc }));
          Ast_helper.Str.include_
            (Ast_helper.Incl.mk
               (Ast_helper.Mod.apply
                  (Ast_helper.Mod.ident { txt = Longident.parse funct; loc })
                  (Ast_helper.Mod.ident
                     { txt = Longident.parse module_name; loc })));
        ]
      in
      let signature =
        List.concat
          [
            [ signature_module module_name signature ];
            [
              Ast_helper.Sig.include_
                (Ast_helper.Incl.mk
                   (Ast_helper.Mty.typeof_
                      (Ast_helper.Mod.mk
                         (Pmod_structure
                            [
                              Ast_helper.Str.include_
                                (Ast_helper.Incl.mk
                                   (Ast_helper.Mod.ident
                                      { txt = Longident.parse module_name; loc }));
                            ]))));
            ];
            [
              Ast_helper.Sig.include_
                (Ast_helper.Incl.mk
                   (Ast_helper.Mty.typeof_
                      (Ast_helper.Mod.mk
                         (Pmod_structure
                            [
                              Ast_helper.Str.include_
                                (Ast_helper.Incl.mk
                                   (Ast_helper.Mod.apply
                                      (Ast_helper.Mod.ident
                                         { txt = Longident.parse funct; loc })
                                      (Ast_helper.Mod.ident
                                         {
                                           txt = Longident.parse module_name;
                                           loc;
                                         })));
                            ]))));
            ];
          ]
      in
      (contents, Ast_helper.Mty.mk ~loc:module_loc (Pmty_signature signature))
    | None -> (contents, module_type)
  in
  match (funct, name) with
  | Some _, Some name ->
    [ wrap_module ~module_type ~loc:module_loc name contents ]
  | Some _, None -> contents
  | None, Some name ->
    [ wrap_module ~module_type ~loc:module_loc name contents ]
  | None, None -> contents

let wrap_query_module_signature ~signature ?(loc = Location.none) definition
  name config =
  let module_name =
    match name with Some name -> name ^ "_inner" | None -> "Inner"
  in
  let funct = get_functor config definition in
  let signature =
    match funct with
    | Some funct ->
      List.concat
        [
          [ signature_module module_name signature ];
          signature;
          [
            Ast_helper.Sig.include_
              (Ast_helper.Incl.mk
                 (Ast_helper.Mty.typeof_
                    (Ast_helper.Mod.mk
                       (Pmod_structure
                          [
                            Ast_helper.Str.include_
                              (Ast_helper.Incl.mk
                                 (Ast_helper.Mod.apply
                                    (Ast_helper.Mod.ident
                                       { txt = Longident.parse funct; loc })
                                    (Ast_helper.Mod.ident
                                       {
                                         txt = Longident.parse module_name;
                                         loc;
                                       })));
                          ]))));
          ];
        ]
    | None -> signature
  in
  match (funct, name) with
  | Some _, Some name -> [ signature_module name signature ]
  | Some _, None -> signature
  | None, Some name -> [ signature_module name signature ]
  | None, None -> signature

let generate_operation_signature config variable_defs res_structure =
  let raw_types =
    match config.native with
    | true -> [ [%sigi: type t] ]
    | false ->
      Output_types.generate_type_signature_items config res_structure true None
        None
  in
  let types =
    Output_types.generate_type_signature_items config res_structure false None
      None
  in
  let raw_arg_types =
    Output_types.generate_arg_type_signature_items true config variable_defs
  in
  let arg_types =
    Output_types.generate_arg_type_signature_items false config variable_defs
  in
  let extracted_args = extract_args config variable_defs in
  let serialize_variable_signatures =
    Output_serializer.generate_serialize_variable_signatures extracted_args
  in
  let variable_constructor_signatures =
    Output_serializer.generate_variable_constructor_signatures extracted_args
  in
  let has_required_variables = has_required_variables extracted_args in
  [
    [ [%sigi: [@@@ocaml.warning "-32-30"]] ];
    [ signature_module "Raw" (List.append raw_types raw_arg_types) ];
    types;
    arg_types;
    [
      wrap_sig_uncurried_fn
        [%sigi:
          val query :
            [%t
              base_type_name
                (match
                   (config.template_tag, config.template_tag_return_type)
                 with
                | (Some _, _, _), Some return_type
                | (_, Some _, _), Some return_type ->
                  return_type
                | _ -> "string")]
            [@@ocaml.doc " The GraphQL query "]];
      wrap_sig_uncurried_fn
        [%sigi:
          val parse : Raw.t -> t
            [@@ocaml.doc
              " Parse the JSON-compatible GraphQL data to ReasonML data types "]];
      wrap_sig_uncurried_fn
        [%sigi:
          val serialize : t -> Raw.t
            [@@ocaml.doc
              " Serialize the ReasonML GraphQL data that was parsed using the \
               parse\n\
               function back to the original JSON compatible data "]];
    ];
    serialize_variable_signatures;
    (match variable_constructor_signatures with
    | [] ->
      [ wrap_sig_uncurried_fn [%sigi: val makeVariables : unit -> t_variables] ]
    | signatures -> signatures);
    (match has_required_variables with
    | true -> []
    | false ->
      [
        wrap_sig_uncurried_fn
          [%sigi: val makeDefaultVariables : unit -> t_variables];
      ]);
    (match config.native with
    | true ->
      [
        [%sigi:
          external unsafe_fromJson : Graphql_ppx_runtime.Json.t -> Raw.t
            = "%identity"];
        [%sigi:
          external toJson : Raw.t -> Graphql_ppx_runtime.Json.t = "%identity"];
        [%sigi:
          external variablesToJson :
            Raw.t_variables -> Graphql_ppx_runtime.Json.t = "%identity"];
      ]
    | false ->
      [
        [%sigi: external unsafe_fromJson : JSON.t -> Raw.t = "%identity"];
        [%sigi: external toJson : Raw.t -> JSON.t = "%identity"];
        [%sigi:
          external variablesToJson : Raw.t_variables -> JSON.t = "%identity"];
      ]);
  ]
  |> List.concat

let rec create_arity_fn arity typ =
  match arity with
  | 0 -> typ
  | arity -> Ast_helper.Typ.arrow Nolabel typ (create_arity_fn (arity - 1) typ)

let graphql_external (config : output_config) _ =
  match config with
  | {
   template_tag = template_tag, location, import;
   template_tag_return_type;
   template_tag_is_function = Some true;
  }
    when location <> None ->
    let return_type =
      match template_tag_return_type with
      | None -> "string"
      | Some return_type -> return_type
    in
    let import =
      match import with None -> "default" | Some import -> import
    in
    [
      Ast_helper.Str.primitive
        (Ast_helper.Val.mk
           ~attrs:
             (match location with
             | None -> []
             | Some location ->
               [
                 Ast_helper.Attr.mk
                   { txt = "module"; loc = Location.none }
                   (PStr [ Ast_helper.Str.eval (const_str_expr location) ]);
                 Ast_helper.Attr.mk
                   { txt = "variadic"; loc = Location.none }
                   (PStr []);
               ])
           ~prim:[ import ]
           {
             txt = template_tag |> Option.get_or_else "graphql";
             loc = Location.none;
           }
           (Ast_helper.Typ.arrow Nolabel [%type: string array]
              (Ast_helper.Typ.arrow Nolabel
                 (base_type ~inner:[ base_type_name return_type ] "array")
                 (base_type_name return_type))));
    ]
  | { template_tag = template_tag, location, import; template_tag_return_type }
    when location <> None ->
    let return_type =
      match template_tag_return_type with
      | None -> "string"
      | Some return_type -> return_type
    in
    let import =
      match (import, location, template_tag) with
      | None, Some _, _ -> "default"
      | Some import, Some _, _ -> import
      | _, None, Some template_tag -> template_tag
      | _, None, None -> assert false
    in
    [
      Ast_helper.Str.primitive
        (Ast_helper.Val.mk
           ~attrs:
             (match location with
             | None ->
               [
                 Ast_helper.Attr.mk
                   { txt = "taggedTemplate"; loc = Location.none }
                   (PStr []);
               ]
             | Some location ->
               [
                 Ast_helper.Attr.mk
                   { txt = "module"; loc = Location.none }
                   (PStr [ Ast_helper.Str.eval (const_str_expr location) ]);
                 Ast_helper.Attr.mk
                   { txt = "taggedTemplate"; loc = Location.none }
                   (PStr []);
               ])
           ~prim:[ import ]
           {
             txt = template_tag |> Option.get_or_else "graphql";
             loc = Location.none;
           }
           (Ast_helper.Typ.arrow Nolabel [%type: string array]
              (Ast_helper.Typ.arrow Nolabel
                 (base_type ~inner:[ base_type_name return_type ] "array")
                 (base_type_name return_type))));
      (*
        this converts a string to the expected return type to make old fragments
        compatible if you use apollo with fragments that don't use the template tag
        this use-case is useful if you have two different GraphQL clients and you want
        to mix the fragments while they are not using the template tag
      *)
      Ast_helper.Str.primitive
        (Ast_helper.Val.mk ~attrs:[] ~prim:[ "%identity" ]
           { txt = "graphql_allow_string"; loc = Location.none }
           (Ast_helper.Typ.arrow Nolabel [%type: string]
              (base_type_name return_type)));
    ]
  | _ -> []

let generate_operation_implementation config variable_defs _has_error operation
  res_structure =
  let parse_fn =
    Output_parser.generate_parser config [] (Graphql_ast.Operation operation)
      res_structure
  in
  let serialize_fn =
    Output_serializer.generate_serializer config []
      (Graphql_ast.Operation operation) None res_structure
  in
  let types =
    Output_types.generate_type_structure_items config res_structure false None
      None
  in
  let raw_types =
    match config.native with
    | true -> [ [%stri type t = Graphql_ppx_runtime.Json.t] ]
    | false ->
      Output_types.generate_type_structure_items config res_structure true None
        None
  in
  let arg_types =
    Output_types.generate_arg_type_structure_items false config variable_defs
  in
  let raw_arg_types =
    Output_types.generate_arg_type_structure_items true config variable_defs
  in
  let extracted_args = extract_args config variable_defs in
  let serialize_variable_functions =
    Output_serializer.generate_serialize_variables extracted_args
  in
  let variable_constructors =
    Output_serializer.generate_variable_constructors extracted_args
  in
  let has_required_variables = has_required_variables extracted_args in
  let printed_query =
    make_printed_query config (Graphql_ast.Operation operation)
  in
  let contents =
    List.concat
      [
        [ [%stri [@@@ocaml.warning "-32-30"]] ];
        [
          wrap_module ~loc:Location.none "Raw"
            (List.append raw_types raw_arg_types);
        ];
        types;
        arg_types;
        graphql_external config (Graphql_ast.Operation operation);
        [
          [%stri let query = [%e printed_query]];
          wrap_as_uncurried_fn [%stri let parse value = [%e parse_fn]];
          wrap_as_uncurried_fn [%stri let serialize value = [%e serialize_fn]];
        ];
        [ serialize_variable_functions ];
        (match variable_constructors with
        | None -> [ wrap_as_uncurried_fn [%stri let makeVariables () = ()] ]
        | Some c -> [ c ]);
        (match has_required_variables with
        | true -> []
        | false ->
          [
            wrap_as_uncurried_fn
              [%stri let makeDefaultVariables () = makeVariables ()];
          ]);
        (match config.native with
        | true ->
          [
            [%stri
              external unsafe_fromJson : Graphql_ppx_runtime.Json.t -> Raw.t
                = "%identity"];
            [%stri
              external toJson : Raw.t -> Graphql_ppx_runtime.Json.t
                = "%identity"];
            [%stri
              external variablesToJson :
                Raw.t_variables -> Graphql_ppx_runtime.Json.t = "%identity"];
          ]
        | false ->
          [
            [%stri external unsafe_fromJson : JSON.t -> Raw.t = "%identity"];
            [%stri external toJson : Raw.t -> JSON.t = "%identity"];
            [%stri
              external variablesToJson : Raw.t_variables -> JSON.t
                = "%identity"];
          ]);
      ]
  in
  let name =
    match operation with
    | { item = { o_name = Some { item = name } } } -> Some name
    | _ -> None
  in
  ( Operation (operation.item.o_type, { has_required_variables }),
    name,
    contents,
    operation.span |> config.map_loc |> conv_loc )

let generate_fragment_signature config name variable_definitions _has_error
  (fragment : Graphql_ast.fragment Source_pos.spanning) type_name res_structure
    =
  let types =
    Output_types.generate_type_signature_items config res_structure false
      type_name
      (Some (fragment.item.fg_type_condition.item, fragment.item.fg_name.span))
  in
  let raw_types =
    match config.native with
    | true -> [ [%sigi: type t] ]
    | false ->
      Output_types.generate_type_signature_items config res_structure true None
        (Some (fragment.item.fg_type_condition.item, fragment.item.fg_name.span))
  in
  let rec make_labeled_fun_sig final_type = function
    | [] -> final_type
    | ({ Source_pos.item = name }, { Graphql_ast.vd_type = { item = type_ref } })
      :: tl ->
      Ast_helper.Typ.arrow (Labelled name)
        (Ast_helper.Typ.variant
           [
             {
               prf_desc =
                 Rtag
                   ( {
                       txt = Output_parser.generate_poly_type_ref_name type_ref;
                       loc = Location.none;
                     },
                     true,
                     [] );
               prf_loc = Location.none;
               prf_attributes = [];
             };
           ]
           Closed None)
        (make_labeled_fun_sig final_type tl)
  in
  let verify_parse =
    make_labeled_fun_sig
      (Ast_helper.Typ.arrow (Labelled "fragmentName")
         (Ast_helper.Typ.variant
            [
              {
                prf_desc = Rtag ({ txt = name; loc = Location.none }, true, []);
                prf_loc = Location.none;
                prf_attributes = [];
              };
            ]
            Closed None)
         (Ast_helper.Typ.arrow Nolabel (base_type_name "Raw.t")
            (base_type_name "t")))
      (match variable_definitions with
      | Some { Source_pos.item = variable_definitions } -> variable_definitions
      | None -> [])
  in
  let verify_name =
    Ast_helper.Typ.arrow Nolabel
      (Ast_helper.Typ.variant
         [
           {
             prf_desc = Rtag ({ txt = name; loc = Location.none }, true, []);
             prf_loc = Location.none;
             prf_attributes = [];
           };
         ]
         Closed None)
      (base_type_name "unit")
  in
  let type_name = base_type_name (Option.get_or_else "t" type_name) in
  List.concat
    [
      [ [%sigi: [@@@ocaml.warning "-32-30"]] ];
      [ signature_module "Raw" raw_types ];
      types;
      [
        [%sigi:
          val query :
            [%t
              base_type_name
                (match
                   (config.template_tag, config.template_tag_return_type)
                 with
                | (Some _, _, _), Some return_type
                | (_, Some _, _), Some return_type ->
                  return_type
                | _ -> "string")]
          [@@ocaml.doc " the GraphQL fragment "]];
        wrap_sig_uncurried_fn
          [%sigi:
            val parse : Raw.t -> [%t type_name]
            [@@ocaml.doc
              " Parse the raw JSON-compatible GraphQL data into ReasonML data \
               types "]];
        wrap_sig_uncurried_fn
          [%sigi:
            val serialize : [%t type_name] -> Raw.t
            [@@ocaml.doc
              " Serialize the ReasonML GraphQL data that was parsed using the \
               parse\n\
               function back to the original JSON-compatible data "]];
        [%sigi: val verifyArgsAndParse : [%t verify_parse]];
        [%sigi: val verifyName : [%t verify_name]];
      ];
      (match config.native with
      | true ->
        [
          [%sigi:
            external unsafe_fromJson : Graphql_ppx_runtime.Json.t -> Raw.t
              = "%identity"];
          [%sigi:
            external toJson : Raw.t -> Graphql_ppx_runtime.Json.t = "%identity"];
        ]
      | false ->
        [
          [%sigi: external unsafe_fromJson : JSON.t -> Raw.t = "%identity"];
          [%sigi: external toJson : Raw.t -> JSON.t = "%identity"];
        ]);
    ]

let generate_fragment_implementation config name
  (variable_definitions :
    Graphql_ast.variable_definitions Source_pos.spanning option) _has_error
  fragment type_name res_structure =
  let parse_fn =
    Output_parser.generate_parser config [] (Graphql_ast.Fragment fragment)
      res_structure
  in
  let serialize_fn =
    Output_serializer.generate_serializer config []
      (Graphql_ast.Fragment fragment) None res_structure
  in
  let types =
    Output_types.generate_type_structure_items config res_structure false
      type_name
      (Some (fragment.item.fg_type_condition.item, fragment.item.fg_name.span))
  in
  let raw_types =
    match config.native with
    | true -> [ [%stri type t = Graphql_ppx_runtime.Json.t] ]
    | false ->
      Output_types.generate_type_structure_items config res_structure true None
        (Some (fragment.item.fg_type_condition.item, fragment.item.fg_name.span))
  in
  let rec make_labeled_fun body = function
    | [] -> body
    | ({ Source_pos.item = name }, { Graphql_ast.vd_type = { item = type_ref } })
      :: tl ->
      Ast_helper.Exp.fun_ (Labelled name) None
        (Ast_helper.Pat.constraint_
           (Ast_helper.Pat.var { txt = "_" ^ name; loc = Location.none })
           (Ast_helper.Typ.variant
              [
                {
                  prf_desc =
                    Rtag
                      ( {
                          txt =
                            Output_parser.generate_poly_type_ref_name type_ref;
                          loc = Location.none;
                        },
                        true,
                        [] );
                  prf_loc = Location.none;
                  prf_attributes = [];
                };
              ]
              Closed None))
        (make_labeled_fun body tl)
  in
  let printed_query =
    make_printed_query config (Graphql_ast.Fragment fragment)
  in
  let verify_parse =
    make_labeled_fun
      (Ast_helper.Exp.fun_ (Labelled "fragmentName") None
         (Ast_helper.Pat.constraint_
            (Ast_helper.Pat.var { txt = "_" ^ name; loc = Location.none })
            (Ast_helper.Typ.variant
               [
                 {
                   prf_desc =
                     Rtag ({ txt = name; loc = Location.none }, true, []);
                   prf_loc = Location.none;
                   prf_attributes = [];
                 };
               ]
               Closed None))
         [%expr fun (value : Raw.t) -> parse value])
      (match variable_definitions with
      | Some { Source_pos.item = variable_definitions } -> variable_definitions
      | None -> [])
  in
  let param_var = { txt = "__param"; loc = Location.none } in
  let param_ident = { txt = Longident.Lident "__param"; loc = Location.none } in
  let verifyName =
    Ast_helper.Exp.fun_ Nolabel None
      (Ast_helper.Pat.var param_var)
      (Ast_helper.Exp.match_
         (Ast_helper.Exp.ident param_ident)
         [
           Ast_helper.Exp.case
             (Ast_helper.Pat.variant name None)
             (Ast_helper.Exp.construct
                { txt = Longident.Lident "()"; loc = Location.none }
                None);
         ])
  in
  let contents =
    [
      [ [%stri [@@@ocaml.warning "-32-30"]] ];
      [ wrap_module ~loc:Location.none "Raw" raw_types ];
      types;
      graphql_external config (Graphql_ast.Fragment fragment);
      [
        [%stri let query = [%e printed_query]];
        wrap_as_uncurried_fn [%stri let parse value = [%e parse_fn]];
        wrap_as_uncurried_fn [%stri let serialize value = [%e serialize_fn]];
      ];
      [
        [%stri let verifyArgsAndParse = [%e verify_parse]];
        [%stri let verifyName = [%e verifyName]];
      ];
      (match config.native with
      | true ->
        [
          [%stri
            external unsafe_fromJson : Graphql_ppx_runtime.Json.t -> Raw.t
              = "%identity"];
          [%stri
            external toJson : Raw.t -> Graphql_ppx_runtime.Json.t = "%identity"];
        ]
      | false ->
        [
          [%stri external unsafe_fromJson : JSON.t -> Raw.t = "%identity"];
          [%stri external toJson : Raw.t -> JSON.t = "%identity"];
        ]);
    ]
    |> List.concat
  in
  ( Fragment,
    Some (Generator_utils.capitalize_ascii name),
    contents,
    fragment.span |> config.map_loc |> conv_loc )

let generate_definition config = function
  | Def_operation
      { variable_definitions = vdefs; has_error; operation; inner = structure }
    ->
    ( generate_operation_implementation config vdefs has_error operation
        structure,
      generate_operation_signature config vdefs structure )
  | Def_fragment
      {
        name;
        variable_definitions;
        has_error;
        fragment;
        type_name;
        inner = structure;
      } -> (
    try
      ( generate_fragment_implementation config name variable_definitions
          has_error fragment type_name structure,
        generate_fragment_signature config name variable_definitions has_error
          fragment type_name structure )
    with
    | Graphql_printer.Cant_find_fragment_type
        (fragment_type : string Source_pos.spanning)
    ->
      raise
        (Cant_find_fragment_type_with_loc
           (config.map_loc fragment_type.span, fragment_type.item)))

let generate_modules module_name module_type operations =
  match operations with
  | [] -> []
  | [ (operation, config) ] -> (
    match generate_definition config operation with
    | (definition, Some name, contents, loc), signature ->
      let module_type =
        match module_type with
        | Some module_type -> module_type
        | None -> Ast_helper.Mty.mk (Pmty_signature signature)
      in
      wrap_query_module ~loc ~module_type definition
        (match (config.inline, module_name) with
        | true, _ -> None
        | _, Some name -> Some name
        | _, None -> Some name)
        contents config
    | (definition, None, contents, loc), signature ->
      let module_type =
        match module_type with
        | Some module_type -> module_type
        | None -> Ast_helper.Mty.mk (Pmty_signature signature)
      in
      wrap_query_module ~loc ~module_type definition module_name contents config
    )
  | operations -> (
    let contents =
      operations
      |> List.map (fun (operation, config) ->
           (generate_definition config operation, config))
      |> List.mapi
           (fun i (((definition, name, contents, loc), signature), config) ->
           let module_type =
             match module_type with
             | Some module_type -> module_type
             | None -> Ast_helper.Mty.mk (Pmty_signature signature)
           in
           match name with
           | Some name ->
             wrap_query_module ~loc ~module_type definition (Some name) contents
               config
           | None ->
             wrap_query_module ~loc ~module_type definition
               (Some ("Untitled" ^ string_of_int i))
               contents config)
      |> List.concat
    in
    match module_name with
    | Some module_name ->
      [ wrap_module ~loc:Location.none module_name contents ]
    | None -> contents)

let generate_module_interfaces module_name operations =
  match operations with
  | [] -> []
  | [ (operation, config) ] -> (
    match generate_definition config operation with
    | (definition, Some name, _contents, _loc), signature ->
      wrap_query_module_signature ~signature definition
        (match (config.inline, module_name) with
        | true, _ -> None
        | _, Some name -> Some name
        | _, None -> Some name)
        config
    | (definition, None, _contents, _loc), signature ->
      wrap_query_module_signature ~signature definition module_name config)
  | operations -> (
    let contents =
      operations
      |> List.map (fun (operation, config) ->
           (generate_definition config operation, config))
      |> List.mapi (fun i (((definition, name, _, _), signature), config) ->
           match name with
           | Some name ->
             wrap_query_module_signature ~signature definition (Some name)
               config
           | None ->
             wrap_query_module_signature ~signature definition
               (Some ("Untitled" ^ string_of_int i))
               config)
      |> List.concat
    in
    match module_name with
    | Some module_name -> [ signature_module module_name contents ]
    | None -> contents)
