open Graphql_compiler
open Source_pos
open Schema
open Ppxlib
open Output_utils
open Extract_type_definitions
open Result_structure
open Output_types

let conv_loc _ = Location.none
let raw_value loc = ([%expr value] [@metaloc loc])

let raw_opaque_object interface_fragments fields =
  let has_fragments =
    fields |> List.exists (function Fr_fragment_spread _ -> true | _ -> false)
  in
  match (has_fragments, interface_fragments) with
  | true, _ -> true
  | _, Some (_, []) -> false
  | _, Some (_, _) -> true
  | _ -> false

let rec list_literal = function
  | [] ->
    Ast_helper.Exp.construct
      { txt = Longident.Lident "[]"; loc = Location.none }
      None
  | value :: values ->
    Ast_helper.Exp.construct
      { txt = Longident.Lident "::"; loc = Location.none }
      (Some (Ast_helper.Exp.tuple [ value; list_literal values ]))

let rec serialize_type = function
  | Type (Scalar { sm_name = "ID" }) | Type (Scalar { sm_name = "String" }) -> (
    match Ppx_config.native () with
    | true -> [%expr fun a -> `String a]
    | false -> [%expr fun a -> a])
  | Type (Scalar { sm_name = "Int" }) -> (
    match Ppx_config.native () with
    | true -> [%expr fun a -> `Int a]
    | false -> [%expr fun a -> a])
  | Type (Scalar { sm_name = "Float" }) -> (
    match Ppx_config.native () with
    | true -> [%expr fun a -> `Float a]
    | false -> [%expr fun a -> a])
  | Type (Scalar { sm_name = "Boolean" }) -> (
    match Ppx_config.native () with
    | true -> [%expr fun a -> `Bool a]
    | false -> [%expr fun a -> a])
  | Type (Scalar { sm_name = _ }) -> [%expr fun a -> a]
  | Type (InputObject { iom_name }) ->
    [%expr
      fun a -> [%e ident_from_string ("serializeInputObject" ^ iom_name)] a]
  | Type (Enum { em_values }) ->
    let case_exp =
      Ast_helper.Exp.match_ (ident_from_string "a")
        (em_values
        |> List.map (fun value ->
             Ast_helper.Exp.case
               (Ast_helper.Pat.variant value.evm_name None)
               (match Ppx_config.native () with
               | true ->
                 [%expr
                   `String
                     [%e
                       Ast_helper.Exp.constant
                         (Parsetree.Pconst_string
                            (value.evm_name, Location.none, None))]]
               | false ->
                 Ast_helper.Exp.constant
                   (Parsetree.Pconst_string (value.evm_name, Location.none, None))))
        )
    in
    [%expr fun a -> [%e case_exp]]
  | Nullable inner -> (
    match Ppx_config.native () with
    | true ->
      [%expr
        fun a ->
          match a with None -> `Null | Some b -> [%e serialize_type inner] b]
    | false ->
      [%expr
        fun a ->
          match a with
          | None -> Js.Nullable.undefined
          | Some b -> Js.Nullable.return ([%e serialize_type inner] b)])
  | List inner -> (
    match Ppx_config.native () with
    | true ->
      [%expr
        fun a ->
          `List
            (Array.map (fun b -> [%e serialize_type inner] b) a |> Array.to_list)]
    | false ->
      [%expr fun a -> Js.Array2.map a (fun b -> [%e serialize_type inner] b)])
  | Type (Object _) -> [%expr fun v -> None]
  | Type (Union _) -> [%expr fun v -> None]
  | Type (Interface _) -> [%expr fun v -> None]
  | TypeNotFound _ -> [%expr fun v -> None]

let record_to_object loc record =
  Ast_helper.Exp.extension
    ({ txt = "bs.obj"; loc = conv_loc loc }, PStr [ [%stri [%e record]] ])

let serialize_fun fields type_name =
  let arg = "inp" in
  let object_ =
    match Ppx_config.native () with
    | true ->
      let assoc_fields =
        List.map
          (fun (InputField { name; type_ }) ->
            [%expr
              [%e const_str_expr name],
                [%e serialize_type type_]
                  [%e
                    Ast_helper.Exp.field
                      (Ast_helper.Exp.constraint_ (ident_from_string arg)
                         (base_type_name type_name))
                      {
                        loc = Location.none;
                        Location.txt = Longident.parse (to_valid_ident name);
                      }]])
          fields
      in
      [%expr `Assoc [%e list_literal assoc_fields]]
    | false ->
      Ast_helper.Exp.record
        (fields
        |> List.map (fun (InputField { name; type_; loc }) ->
             let loc = conv_loc loc in
             ( { txt = Longident.parse (to_valid_ident name); loc },
               [%expr
                 [%e serialize_type type_]
                   [%e
                     Ast_helper.Exp.field
                       (Ast_helper.Exp.constraint_ (ident_from_string arg)
                          (base_type_name type_name))
                       {
                         loc = Location.none;
                         Location.txt = Longident.parse (to_valid_ident name);
                       }]] )))
        None
  in
  Ast_helper.Exp.fun_ Nolabel None
    (Ast_helper.Pat.var ~loc:Location.none { txt = arg; loc = Location.none })
    object_

let is_recursive input_objects = List.length input_objects > 1

let filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with None -> aux accu l | Some v -> aux (v :: accu) l)
  in
  aux []

let generate_serialize_variable_signatures (arg_type_defs : arg_type_def list) =
  match arg_type_defs with
  | [ NoVariables ] -> [%sig: val serializeVariables : unit -> Raw.t_variables]
  | arg_type_defs ->
    arg_type_defs
    |> filter_map (function
         | InputObject { name; loc } -> Some (name, loc)
         | NoVariables -> None)
    |> List.map (fun (name, loc) ->
         let type_name =
           match name with
           | None -> "t_variables"
           | Some input_object_name -> "t_variables_" ^ input_object_name
         in
         Ast_helper.Sig.value
           (Ast_helper.Val.mk
              {
                loc = conv_loc loc;
                txt =
                  (match name with
                  | None -> "serializeVariables"
                  | Some input_object_name ->
                    "serializeInputObject" ^ input_object_name);
              }
              (Ast_helper.Typ.arrow ~loc:(conv_loc loc) Nolabel
                 (base_type_name type_name)
                 (base_type_name ("Raw." ^ type_name)))))

let generate_serialize_variables (arg_type_defs : arg_type_def list) =
  match arg_type_defs with
  | [ NoVariables ] -> (
    match Ppx_config.native () with
    | true -> [%stri let serializeVariables () = `Null]
    | false -> [%stri let serializeVariables () = ()])
  | arg_type_defs ->
    Ast_helper.Str.value
      (match is_recursive arg_type_defs with
      | true -> Recursive
      | false -> Nonrecursive)
      (arg_type_defs
      |> filter_map (function
           | InputObject { name; fields; loc } -> Some (name, fields, loc)
           | NoVariables -> None)
      |> List.map (fun (name, fields, loc) ->
           let type_name =
             match name with
             | None -> "t_variables"
             | Some input_object_name -> "t_variables_" ^ input_object_name
           in
           (Ast_helper.Vb.mk
              (Ast_helper.Pat.constraint_ ~loc:(conv_loc loc)
                 (Ast_helper.Pat.var
                    {
                      loc = conv_loc loc;
                      txt =
                        (match name with
                        | None -> "serializeVariables"
                        | Some input_object_name ->
                          "serializeInputObject" ^ input_object_name);
                    })
                 (Ast_helper.Typ.arrow ~loc:(conv_loc loc) Nolabel
                    (base_type_name type_name)
                    (base_type_name ("Raw." ^ type_name))))
              (serialize_fun fields type_name) [@metaloc conv_loc loc])))

let generate_variable_constructors (arg_type_defs : arg_type_def list) =
  match arg_type_defs with
  | [ NoVariables ] -> None
  | _ ->
    Some
      (Ast_helper.Str.value Nonrecursive
         (arg_type_defs
         |> filter_map (function
              | InputObject { name; fields; loc } -> Some (name, fields, loc)
              | NoVariables -> None)
         |> List.map (fun (name, fields, loc) ->
              let loc = conv_loc loc in
              let rec make_labeled_fun body = function
                | [] -> [%expr fun () -> [%e body]] [@metaloc loc]
                | InputField { name; loc; type_ } :: tl ->
                  let name_loc = loc |> conv_loc in
                  Ast_helper.Exp.fun_ ~loc:name_loc
                    (match type_ with
                    | List _ | Type _ -> Labelled (to_valid_ident name)
                    | _ -> Optional (to_valid_ident name))
                    None
                    (Ast_helper.Pat.var ~loc:name_loc
                       { txt = to_valid_ident name; loc = name_loc })
                    (make_labeled_fun body tl)
              in
              let object_ =
                Ast_helper.Exp.record ~loc:(loc |> conv_loc)
                  (fields
                  |> List.map (fun (InputField { name; loc }) ->
                       ( {
                           Location.txt = Longident.parse (to_valid_ident name);
                           loc = conv_loc loc;
                         },
                         ident_from_string (to_valid_ident name) )))
                  None
              in
              let body =
                Ast_helper.Exp.constraint_ ~loc:(conv_loc loc) object_
                  (base_type_name
                     (match name with
                     | None -> "t_variables"
                     | Some input_type_name -> "t_variables_" ^ input_type_name))
              in
              match name with
              | None ->
                let make_variables_body = make_labeled_fun body fields in
                [ (name, loc, make_variables_body) ]
              | Some _ -> [ (name, loc, make_labeled_fun body fields) ])
         |> List.concat
         |> List.map (fun (name, loc, expr) ->
              (Ast_helper.Vb.mk
                 (Ast_helper.Pat.var
                    {
                      loc = conv_loc loc;
                      txt =
                        (match name with
                        | None -> "makeVariables"
                        | Some "make" -> "make"
                        | Some input_object_name ->
                          "makeInputObject" ^ input_object_name);
                    })
                 expr [@metaloc conv_loc loc]))))

let generate_variable_constructor_signatures (arg_type_defs : arg_type_def list)
    =
  match arg_type_defs with
  | [ NoVariables ] -> []
  | _ ->
    arg_type_defs
    |> filter_map (function
         | InputObject { name; fields; loc } -> Some (name, fields, loc)
         | NoVariables -> None)
    |> List.map (fun (name, fields, loc) ->
         let rec make_labeled_fun final_type = function
           | [] -> final_type
           | InputField { name; loc; type_ } :: tl ->
             Ast_helper.Typ.arrow
               (match type_ with
               | List _ | Type _ -> Labelled (to_valid_ident name)
               | _ -> Optional (to_valid_ident name))
               (generate_arg_type ~nulls:false false loc type_)
               (make_labeled_fun final_type tl)
         in
         let final_type =
           Ast_helper.Typ.arrow Nolabel (base_type_name "unit")
             (base_type_name
                (match name with
                | None -> "t_variables"
                | Some input_type_name -> "t_variables_" ^ input_type_name))
         in
         (name, loc, make_labeled_fun final_type fields))
    |> List.map (fun (name, loc, type_) ->
         Ast_helper.Sig.value
           (Ast_helper.Val.mk
              {
                loc = conv_loc loc;
                txt =
                  (match name with
                  | None -> "makeVariables"
                  | Some "make" -> "make"
                  | Some input_object_name ->
                    "makeInputObject" ^ input_object_name);
              }
              type_))

let get_field key existing_record path =
  [%expr
    [%e
      Ast_helper.Exp.field
        (Ast_helper.Exp.constraint_
           (ident_from_string "value")
           (base_type_name
              (match existing_record with
              | None -> generate_type_name path
              | Some existing -> existing)))
        { loc = Location.none; txt = Longident.parse (to_valid_ident key) }]]

let rec generate_nullable_encoder config loc inner path definition =
  match Ppx_config.native () with
  | true ->
    [%expr
      match value with
      | Some value -> [%e generate_serializer config path definition None inner]
      | None -> `Null]
    [@metaloc loc]
  | false ->
    [%expr
      match value with
      | Some value ->
        Js.Nullable.return
          [%e generate_serializer config path definition None inner]
      | None -> Js.Nullable.null]
    [@metaloc loc]

and generate_array_encoder config loc inner path definition =
  match Ppx_config.native () with
  | true ->
    [%expr
      `List
        (value
        |> Array.map (fun value ->
             [%e generate_serializer config path definition None inner])
        |> Array.to_list)]
    [@metaloc loc]
  | false ->
    [%expr
      Js.Array2.map value (fun value ->
        [%e generate_serializer config path definition None inner])]
    [@metaloc loc]

and generate_poly_enum_encoder loc enum_meta omit_future_value =
  let enum_match_arms =
    enum_meta.em_values
    |> List.map (fun { evm_name; _ } ->
         Ast_helper.Exp.case
           (Ast_helper.Pat.variant evm_name None)
           (match Ppx_config.native () with
           | true -> [%expr `String [%e const_str_expr evm_name]]
           | false -> const_str_expr evm_name))
  in
  let fallback_arm =
    Ast_helper.Exp.case
      (Ast_helper.Pat.variant "FutureAddedValue"
         (Some (Ast_helper.Pat.var { loc; txt = "other" })))
      (match Ppx_config.native () with
      | true -> [%expr `String [%e ident_from_string "other"]]
      | false -> ident_from_string "other")
  in
  let match_expr =
    Ast_helper.Exp.match_ [%expr value]
      (List.concat
         [
           enum_match_arms;
           (match omit_future_value with
           | true -> []
           | false -> [ fallback_arm ]);
         ])
  in
  [%expr [%e match_expr]]

and generate_custom_encoder config loc ident inner path definition =
  ([%expr
     let value = [%e ident_from_string (ident ^ ".serialize")] value in
     [%e generate_serializer config path definition None inner]]
  [@metaloc loc])

and generate_object_encoder config loc _name fields path definition
  existing_record typename interface_fragments =
  let is_opaque = raw_opaque_object interface_fragments fields in
  let do_obj_constructor_base wrap =
    match
      fields
      |> filter_map (function
           | Fr_fragment_spread _ -> None
           | Fr_named_field { name; type_ } -> Some (name, type_))
    with
    | [] -> (
      match Ppx_config.native () with
      | true -> [%expr `Assoc []]
      | false -> [%expr Js.Dict.empty])
    | fields ->
      let record =
        Ast_helper.Exp.record
          (let fields =
             if
               typename <> None
               && not
                    (fields
                    |> List.exists (function
                         | "__typename", _ -> true
                         | _ -> false))
             then
               ("__typename", Res_string { loc = conv_loc_from_ast loc })
               :: fields
             else fields
           in
           fields
           |> List.map (fun (key, _inner) ->
                let key_value =
                  { Location.txt = Longident.parse (to_valid_ident key); loc }
                in
                match (key, typename) with
                | "__typename", Some typename ->
                  (key_value, const_str_expr typename)
                | _ -> (key_value, ident_from_string (to_valid_ident key))))
          None
      in
      let record =
        match wrap with true -> record_to_object loc record | false -> record
      in
      let bindings =
        fields
        |> List.map (fun (key, inner) ->
             Ast_helper.Vb.mk
               (Ast_helper.Pat.var { txt = to_valid_ident key; loc })
               [%expr
                 let value = [%e get_field key existing_record path] in
                 [%e
                   generate_serializer config (key :: path) definition None
                     inner]])
        |> List.rev
      in
      Ast_helper.Exp.let_ Nonrecursive bindings record
  in
  let do_json_encoder () =
    match
      fields
      |> filter_map (function
           | Fr_fragment_spread _ -> None
           | Fr_named_field { name; type_ } -> Some (name, type_))
    with
    | [] -> [%expr `Assoc []]
    | fields ->
      let assoc_fields =
        (if
         typename <> None
         && not
              (fields
              |> List.exists (function "__typename", _ -> true | _ -> false))
        then
         ("__typename", Res_string { loc = conv_loc_from_ast loc }) :: fields
        else fields)
        |> List.map (fun (key, _inner) ->
             match (key, typename) with
             | "__typename", Some typename ->
               [%expr "__typename", `String [%e const_str_expr typename]]
             | key, _ ->
               [%expr
                 [%e const_str_expr key],
                   [%e ident_from_string (to_valid_ident key)]])
      in
      let assoc = [%expr `Assoc [%e list_literal assoc_fields]] in
      let bindings =
        fields
        |> List.map (fun (key, inner) ->
             Ast_helper.Vb.mk
               (Ast_helper.Pat.var { txt = to_valid_ident key; loc })
               [%expr
                 let value = [%e get_field key existing_record path] in
                 [%e
                   generate_serializer config (key :: path) definition None
                     inner]])
        |> List.rev
      in
      Ast_helper.Exp.let_ Nonrecursive bindings assoc
  in
  let do_obj_constructor () =
    (do_obj_constructor_base true [@metaloc conv_loc loc])
  in
  let do_obj_constructor_records () =
    match Ppx_config.native () with
    | true -> do_json_encoder ()
    | false ->
      Ast_helper.Exp.constraint_
        (do_obj_constructor_base false)
        (base_type_name ("Raw." ^ generate_type_name path))
      [@metaloc conv_loc loc]
  in
  let merge_into_opaque () =
    let fields =
      fields
      |> List.fold_left
           (fun acc -> function
             | Fr_named_field _ -> acc
             | Fr_fragment_spread { key; name } ->
               (match Ppx_config.native () with
               | true ->
                 [%expr
                   [%e ident_from_string (name ^ ".toJson")]
                     ([%e ident_from_string (name ^ ".serialize")]
                        [%e get_field key existing_record path])]
               | false ->
                 [%expr
                   (Obj.magic
                      ([%e ident_from_string (name ^ ".serialize")]
                         [%e get_field key existing_record path])
                     : Js.Json.t)])
               :: acc)
           []
      |> List.rev
    in
    let fields =
      match interface_fragments with
      | None | Some (_, []) -> fields
      | Some (interface_name, fragments) ->
        (match Ppx_config.native () with
        | true ->
          [%expr
            let value = [%e get_field "fragment" None path] in
            [%e
              generate_poly_variant_interface_encoder config loc interface_name
                fragments path definition]]
        | false ->
          [%expr
            let value = [%e get_field "fragment" None path] in
            (Obj.magic
               [%e
                 generate_poly_variant_interface_encoder config loc
                   interface_name fragments path definition]
              : Js.Json.t)])
        :: fields
    in
    match Ppx_config.native () with
    | true ->
      [%expr
        Array.fold_left Graphql_ppx_runtime.deepMerge [%e do_obj_constructor ()]
          [%e fields |> Ast_helper.Exp.array]]
    | false ->
      [%expr
        (Obj.magic
           (Js.Array2.reduce
              [%e fields |> Ast_helper.Exp.array]
              Graphql_ppx_runtime.deepMerge
              (Obj.magic [%e do_obj_constructor ()] : Js.Json.t))
          : [%t base_type_name ("Raw." ^ generate_type_name path)])]
  in
  match is_opaque with
  | true -> merge_into_opaque ()
  | false -> do_obj_constructor_records ()

and generate_poly_variant_union_encoder config _loc _name fragments _exhaustive
  omit_future_value path definition =
  let fragment_cases =
    fragments
    |> List.map (fun (({ item = type_name } : Result_structure.name), inner) ->
         Ast_helper.Exp.case
           (Ast_helper.Pat.variant type_name
              (Some (Ast_helper.Pat.var { txt = "value"; loc = Location.none })))
           (match Ppx_config.native () with
           | true -> (
             let expr =
               generate_serializer config (type_name :: path) definition
                 (Some type_name) inner
             in
             match inner with
             | Res_solo_fragment_spread _ ->
               [%expr
                 Graphql_ppx_runtime.assign_typename [%e expr]
                   [%e const_str_expr type_name]]
             | _ -> expr)
           | false -> (
             let raw_type_name =
               base_type_name ("Raw." ^ generate_type_name path)
             in
             let expr =
               [%expr
                 [%e
                   generate_serializer config (type_name :: path) definition
                     (Some type_name) inner]]
             in
             match inner with
             | Res_solo_fragment_spread _ ->
               [%expr
                 (Obj.magic
                    (Graphql_ppx_runtime.assign_typename
                       (Obj.magic [%e expr] : Js.Json.t)
                       [%e const_str_expr type_name])
                   : [%t raw_type_name])]
             | _ -> [%expr (Obj.magic [%e expr] : [%t raw_type_name])])))
  in
  let fallback_case =
    Ast_helper.Exp.case
      (Ast_helper.Pat.variant "FutureAddedValue"
         (Some (Ast_helper.Pat.var { txt = "value"; loc = Location.none })))
      (match Ppx_config.native () with
      | true -> [%expr value]
      | false ->
        [%expr
          (Obj.magic value
            : [%t base_type_name ("Raw." ^ generate_type_name path)])])
  in
  let typename_matcher =
    Ast_helper.Exp.match_ [%expr value]
      (List.concat
         [
           fragment_cases;
           (match omit_future_value with
           | true -> []
           | false -> [ fallback_case ]);
         ])
  in
  [%expr [%e typename_matcher]]

and generate_poly_variant_selection_set_encoder _config _loc _name _fields _path
  _definition =
  let e =
    match Ppx_config.native () with
    | true -> [%expr `Null]
    | false -> [%expr Obj.magic Js.Json.null]
  in
  [%expr
    let _temp = value in
    [%e e]]

and generate_poly_variant_interface_encoder config _loc name fragments path
  definition =
  let fragment_cases =
    fragments
    |> List.map (fun (type_name, inner) ->
         Ast_helper.Exp.case
           (Ast_helper.Pat.variant type_name
              (Some (Ast_helper.Pat.var { txt = "value"; loc = Location.none })))
           (match Ppx_config.native () with
           | true ->
             generate_serializer config
               (type_name :: name :: path)
               definition (Some type_name) inner
           | false ->
             [%expr
               (Obj.magic
                  [%e
                    generate_serializer config
                      (type_name :: name :: path)
                      definition (Some type_name) inner]
                 : [%t
                     base_type_name ("Raw." ^ generate_type_name (name :: path))])]))
  in
  let fallback_case =
    Ast_helper.Exp.case
      (Ast_helper.Pat.variant "UnspecifiedFragment"
         (Some (Ast_helper.Pat.any ())))
      (match Ppx_config.native () with
      | true -> [%expr `Assoc []]
      | false ->
        [%expr
          (Obj.magic (Js.Dict.empty ())
            : [%t base_type_name ("Raw." ^ generate_type_name (name :: path))])])
  in
  let typename_matcher =
    Ast_helper.Exp.match_ [%expr value]
      (List.concat [ fragment_cases; [ fallback_case ] ])
  in
  [%expr [%e typename_matcher]]

and generate_solo_fragment_spread_encorder _config _loc name _arguments
  _definition =
  match Ppx_config.native () with
  | true ->
    [%expr
      [%e ident_from_string (name ^ ".toJson")]
        ([%e ident_from_string (name ^ ".serialize")]
           [%e ident_from_string "value"])]
  | false ->
    [%expr
      [%e ident_from_string (name ^ ".serialize")]
        [%e ident_from_string "value"]]

and generate_error loc message =
  let loc = Output_utils.conv_loc loc in
  let ext =
    Ocaml_common.Ast_mapper.extension_of_error
      (Ocaml_common.Location.error ~loc message)
  in
  let extension = Ocaml_common.Ast_helper.Exp.extension ~loc ext in
  [%expr
    let _value = value in
    [%e To_ppxlib.copy_expression extension]]

and generate_serializer (config : Generator_utils.output_config)
  (path : string list) (definition : Graphql_ast.definition) typename = function
  | Res_nullable { loc; inner } ->
    generate_nullable_encoder config (conv_loc loc) inner path definition
  | Res_array { loc; inner } ->
    generate_array_encoder config (conv_loc loc) inner path definition
  | Res_id { loc } -> (
    let loc = conv_loc loc in
    match Ppx_config.native () with
    | true -> [%expr `String value] [@metaloc loc]
    | false -> raw_value loc)
  | Res_string { loc } -> (
    let loc = conv_loc loc in
    match Ppx_config.native () with
    | true -> [%expr `String value] [@metaloc loc]
    | false -> raw_value loc)
  | Res_int { loc } -> (
    let loc = conv_loc loc in
    match Ppx_config.native () with
    | true -> [%expr `Int value] [@metaloc loc]
    | false -> raw_value loc)
  | Res_float { loc } -> (
    let loc = conv_loc loc in
    match Ppx_config.native () with
    | true -> [%expr `Float value] [@metaloc loc]
    | false -> raw_value loc)
  | Res_boolean { loc } -> (
    let loc = conv_loc loc in
    match Ppx_config.native () with
    | true -> [%expr `Bool value] [@metaloc loc]
    | false -> raw_value loc)
  | Res_raw_scalar { loc } -> raw_value (conv_loc loc)
  | Res_poly_enum { loc; enum_meta; omit_future_value } ->
    generate_poly_enum_encoder (conv_loc loc) enum_meta omit_future_value
  | Res_custom_decoder { loc; ident; inner } ->
    generate_custom_encoder config (conv_loc loc) ident inner path definition
  | Res_record
      { loc; name; fields; type_name = existing_record; interface_fragments }
  | Res_object
      { loc; name; fields; type_name = existing_record; interface_fragments } ->
    generate_object_encoder config (conv_loc loc) name fields path definition
      existing_record typename interface_fragments
  | Res_poly_variant_union
      { loc; name; fragments; exhaustive; omit_future_value } ->
    generate_poly_variant_union_encoder config (conv_loc loc) name fragments
      exhaustive omit_future_value path definition
  | Res_poly_variant_selection_set { loc; name; fragments = fields } ->
    generate_poly_variant_selection_set_encoder config (conv_loc loc) name
      fields path definition
  | Res_poly_variant_interface { loc; name; fragments } ->
    generate_poly_variant_interface_encoder config (conv_loc loc) name fragments
      path definition
  | Res_solo_fragment_spread { loc; name; arguments } ->
    generate_solo_fragment_spread_encorder config (conv_loc loc) name arguments
      definition
  | Res_error { loc; message } -> generate_error loc message
