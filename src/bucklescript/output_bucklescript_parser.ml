open Graphql_ppx_base
open Result_structure
open Schema
open Ppxlib
open Generator_utils
open Output_bucklescript_utils
open Output_bucklescript_types
open Extract_type_definitions

let conv_loc _ = Location.none

let record_to_object loc record =
  let open Ast_helper in
  Exp.extension
    ({ txt = "bs.obj"; loc = conv_loc loc }, PStr [ [%stri [%e record]] ])

let raw_opaque_object interface_fragments fields =
  let has_fragments =
    fields |> List.exists (function Fr_fragment_spread _ -> true | _ -> false)
  in
  match (has_fragments, interface_fragments) with
  | true, _ -> true
  | _, Some (_, []) -> false
  | _, Some (_, _) -> true
  | _ -> false

let rec generate_poly_type_ref_name (type_ref : Graphql_ast.type_ref) =
  match type_ref with
  | Tr_named { item = name } -> name
  | Tr_list { item = type_ref } ->
      "ListOf_" ^ generate_poly_type_ref_name type_ref
  | Tr_non_null_named { item = name } -> "NonNull_" ^ name
  | Tr_non_null_list { item = type_ref } ->
      "NonNullListOf_" ^ generate_poly_type_ref_name type_ref

let get_variable_definitions (definition : Graphql_ast.definition) =
  match definition with
  | Fragment
      { item = { fg_variable_definitions = Some { item = definitions } } }
  | Operation
      { item = { o_variable_definitions = Some { item = definitions } } } ->
      let open Graphql_ast in
      definitions
      |> List.fold_left
           (fun acc
                ( { Source_pos.item = name; span },
                  { vd_type = { item = type_ref; span = type_span } } ) ->
             (name, generate_poly_type_ref_name type_ref, span, type_span)
             :: acc)
           []
  | _ -> []

let make_error_raiser message =
  if Ppx_config.native () then
    if Ppx_config.verbose_error_handling () then
      [%expr raise (Failure ("graphql-ppx: " ^ [%e message]))]
    else [%expr raise (Failure "Unexpected GraphQL query response")]
  else if Ppx_config.verbose_error_handling () then
    [%expr Js.Exn.raiseError ("graphql-ppx: " ^ [%e message])]
  else [%expr Js.Exn.raiseError "Unexpected GraphQL query response"]

let raw_value loc = ([%expr value] [@metaloc loc])

let generate_poly_enum_decoder loc enum_meta omit_future_value =
  let enum_match_arms =
    let open Ast_helper in
    enum_meta.em_values
    |> List.map (fun { evm_name; _ } ->
           Exp.case (const_str_pat evm_name)
             (Exp.variant (to_valid_ident evm_name) None))
  in
  let fallback_arm =
    match omit_future_value with
    | true ->
        let open Ast_helper in
        Exp.case (Pat.any ())
          (Exp.apply
             (Exp.ident (mknoloc (Longident.parse "raise")))
             [
               ( Nolabel,
                 Exp.construct (mknoloc (Longident.parse "Not_found")) None );
             ])
    | false ->
        let open Ast_helper in
        Exp.case
          (Pat.var { loc = conv_loc loc; txt = "other" })
          (Exp.variant "FutureAddedValue"
             (Some
                (Exp.ident
                   {
                     Location.txt = Longident.parse "other";
                     loc = conv_loc loc;
                   })))
  in
  let match_expr =
    let open Ast_helper in
    Exp.match_
      (match Ppx_config.native () with
      | true ->
          [%expr value |> Yojson.Basic.Util.to_string] [@metaloc conv_loc loc]
      | false -> [%expr (Obj.magic value : string)] [@metaloc conv_loc loc])
      (List.concat [ enum_match_arms; [ fallback_arm ] ])
  in
  [%expr [%e match_expr]]

let generate_fragment_parse_fun config loc name arguments definition =
  let open Ast_helper in
  let ident =
    Ast_helper.Exp.ident
      {
        loc = loc |> Output_bucklescript_utils.conv_loc;
        txt = Longident.parse (name ^ ".verifyArgsAndParse");
      }
  in
  let variable_defs = get_variable_definitions definition in
  let labeled_args =
    variable_defs
    |> List.filter (fun (name, _, _, _) ->
           arguments |> List.exists (fun arg -> arg = name))
    |> List.map (fun (arg_name, type_, _span, type_span) ->
           ( Labelled arg_name,
             Exp.variant
               ~loc:
                 (config.map_loc type_span |> Output_bucklescript_utils.conv_loc)
               type_ None ))
  in
  Ast_helper.Exp.apply
    ~loc:(loc |> Output_bucklescript_utils.conv_loc)
    ident
    (List.append labeled_args
       [
         ( Labelled "fragmentName",
           Exp.variant
             ~loc:(loc |> Output_bucklescript_utils.conv_loc)
             name None );
         ( Nolabel,
           match config.native with
           | true ->
               Ast_helper.Exp.apply
                 (Ast_helper.Exp.ident
                    {
                      loc = Location.none;
                      txt = Longident.parse (name ^ ".unsafe_fromJson");
                    })
                 [ (Nolabel, ident_from_string "value") ]
           | false -> ident_from_string ~loc "value" );
       ])

let generate_solo_fragment_spread_decoder config loc name arguments definition =
  generate_fragment_parse_fun config loc name arguments definition

let generate_error loc message =
  let loc = conv_loc loc in
  let error = Ocaml_common.Location.error ~loc message in
  let ext = Ocaml_common.Ast_mapper.extension_of_error error in
  let extension = Ocaml_common.Ast_helper.Exp.extension ~loc ext in
  [%expr
    let _value = value in
    [%e To_ppxlib.copy_expression extension]]

let rec generate_nullable_decoder config loc inner path definition =
  match config.native with
  | true ->
      [%expr
        match value with
        | `Null -> None
        | value -> Some [%e generate_parser config path definition inner]]
      [@metaloc loc]
  | false ->
      [%expr
        match Js.toOption value with
        | Some value -> Some [%e generate_parser config path definition inner]
        | None -> None]
      [@metaloc loc]

and generate_array_decoder config loc inner path definition =
  match config.native with
  | true ->
      [%expr
        match value with
        | `List json_list ->
            List.map
              (fun value -> [%e generate_parser config path definition inner])
              json_list
            |> Array.of_list
        | _ -> [||]]
      [@metaloc loc]
  | false ->
      [%expr
        value
        |> Js.Array.map (fun value ->
               [%e generate_parser config path definition inner])]
      [@metaloc loc]

and generate_custom_decoder config loc ident inner path definition =
  ([%expr
     [%e ident_from_string (ident ^ ".parse")]
       [%e generate_parser config path definition inner]]
  [@metaloc loc])

and generate_object_decoder ~config ~loc ~name:_name ~path ~definition
    ~existing_record ~interface_fragments fields =
  let do_obj_constructor_base () =
    let open Ast_helper in
    let opaque = raw_opaque_object interface_fragments fields in
    let object_type = base_type_name ("Raw." ^ generate_type_name path) in
    let get_value = function
      | Fr_named_field { name = key; type_ = inner } -> (
          match config.native with
          | true ->
              [%expr
                let value =
                  Yojson.Basic.Util.member [%e const_str_expr key] value
                in
                [%e generate_parser config (key :: path) definition inner]]
              [@metaloc conv_loc loc]
          | false ->
              [%expr
                let value =
                  [%e
                    match opaque with
                    | true ->
                        [%expr
                          Obj.magic
                            (Js.Dict.unsafeGet (Obj.magic value)
                               [%e const_str_expr key])]
                    | false ->
                        Ast_helper.Exp.field
                          (Exp.constraint_
                             (ident_from_string "value")
                             object_type)
                          {
                            loc = Location.none;
                            Location.txt = Longident.parse (to_valid_ident key);
                          }]
                in
                [%e generate_parser config (key :: path) definition inner]]
              [@metaloc conv_loc loc])
      | Fr_fragment_spread { loc; name; arguments } -> (
          match Ppx_config.native () with
          | true ->
              generate_fragment_parse_fun config loc name arguments definition
          | false ->
              let our_loc = loc in
              let loc = conv_loc loc in
              ([%expr
                 let value =
                   (Obj.magic value
                     : [%t base_type_name ~loc (name ^ ".Raw.t")])
                 in
                 [%e
                   generate_fragment_parse_fun config our_loc name arguments
                     definition]]
              [@metaloc loc]))
    in
    let get_record_contents_inline = function
      | (Fr_fragment_spread { key } as field)
      | (Fr_named_field { name = key } as field) ->
          ( { txt = Longident.parse (to_valid_ident key); loc = conv_loc loc },
            get_value field )
    in
    let record_fields = List.map get_record_contents_inline fields in
    let record_fields =
      match interface_fragments with
      | None | Some (_, []) -> record_fields
      | Some (interface_name, fragments) ->
          ( { txt = Longident.Lident "fragment"; loc = Location.none },
            generate_poly_variant_interface_decoder config (conv_loc loc)
              interface_name fragments (interface_name :: path) definition )
          :: record_fields
    in
    Exp.record record_fields None
  in
  ((let open Ast_helper in
   Exp.constraint_
     (do_obj_constructor_base ())
     (base_type_name
        (match existing_record with
        | None -> generate_type_name path
        | Some type_name -> type_name))) [@metaloc loc])

and generate_poly_variant_selection_set_decoder config loc name fields path
    definition =
  let rec generator_loop = function
    | (({ item = field } : Result_structure.name), inner) :: next -> (
        let field_name = String.capitalize_ascii field in
        let variant_decoder =
          let open Ast_helper in
          Exp.variant field_name
            (Some (generate_parser config (field :: path) definition inner))
        in
        match config.native with
        | true ->
            [%expr
              let temp =
                Yojson.Basic.Util.member [%e const_str_expr field] value
              in
              match temp with
              | `Null -> [%e generator_loop next]
              | _ ->
                  let value = temp in
                  [%e variant_decoder]]
            [@metaloc loc]
        | false ->
            [%expr
              let temp =
                Js.Dict.unsafeGet (Obj.magic value) [%e const_str_expr field]
              in
              match Js.Json.decodeNull temp with
              | None ->
                  let value = temp in
                  [%e variant_decoder]
              | Some _ -> [%e generator_loop next]]
            [@metaloc loc])
    | [] ->
        make_error_raiser
          [%expr
            "All fields on variant selection set on type "
            ^ [%e const_str_expr name] ^ " were null"]
  in
  match config.native with
  | true ->
      [%expr
        match value with
        | `Null ->
            [%e
              make_error_raiser
                [%expr
                  "Expected type " ^ [%e const_str_expr name]
                  ^ " to be an object"]]
        | value -> [%e generator_loop fields]]
      [@metaloc loc]
  | false ->
      [%expr
        match Js.Json.decodeObject (Obj.magic value : Js.Json.t) with
        | None ->
            [%e
              make_error_raiser
                [%expr
                  "Expected type " ^ [%e const_str_expr name]
                  ^ " to be an object"]]
        | Some value -> [%e generator_loop fields]]
      [@metaloc loc]

and generate_poly_variant_interface_decoder config loc _name fragments path
    definition =
  let fallback_case =
    let open Ast_helper in
    let name_pattern = Pat.any () in
    Exp.variant "UnspecifiedFragment" (Some [%expr typename])
    |> Exp.case name_pattern
  in
  let fragment_cases =
    List.map
      (fun (type_name, inner) ->
        let open Ast_helper in
        let name_pattern = const_str_pat type_name in
        Exp.variant type_name
          (Some
             (match config.native with
             | true ->
                 generate_parser config (type_name :: path) definition inner
             | false ->
                 [%expr
                   let value =
                     (Obj.magic value
                       : [%t
                           base_type_name
                             ("Raw." ^ generate_type_name (type_name :: path))])
                   in
                   [%e
                     generate_parser config (type_name :: path) definition inner]]))
        |> Exp.case name_pattern)
      fragments
  in
  let fallback_case = fallback_case in
  let typename_matcher =
    let open Ast_helper in
    Exp.match_ [%expr typename]
      (List.concat [ fragment_cases; [ fallback_case ] ])
  in
  match config.native with
  | true ->
      [%expr
        let typename =
          (value
           |> Yojson.Basic.Util.member "__typename"
           |> Yojson.Basic.Util.to_string
            : string)
        in
        ([%e typename_matcher] : [%t base_type_name (generate_type_name path)])]
      [@metaloc loc]
  | false ->
      [%expr
        let typename =
          (Obj.magic (Js.Dict.unsafeGet (Obj.magic value) "__typename")
            : string)
        in
        ([%e typename_matcher] : [%t base_type_name (generate_type_name path)])]
      [@metaloc loc]

and generate_poly_variant_union_decoder config loc _name fragments
    _exhaustive_flag omit_future_value path definition =
  let fragment_cases =
    fragments
    |> List.map (fun (({ item = type_name } : Result_structure.name), inner) ->
           let open Ast_helper in
           Exp.case (const_str_pat type_name)
             (Exp.variant type_name
                (Some
                   (match config.native with
                   | true ->
                       generate_parser config (type_name :: path) definition
                         inner
                   | false ->
                       [%expr
                         let value =
                           (Obj.magic value
                             : [%t
                                 match inner with
                                 | Res_solo_fragment_spread { name } ->
                                     base_type_name (name ^ ".Raw.t")
                                 | _ ->
                                     base_type_name
                                       ("Raw."
                                       ^ generate_type_name (type_name :: path)
                                       )])
                         in
                         [%e
                           generate_parser config (type_name :: path) definition
                             inner]]))))
  in
  let fallback_case =
    if omit_future_value then
      let open Ast_helper in
      Exp.case (Pat.any ())
        (Exp.apply
           (Exp.ident (mknoloc (Longident.parse "raise")))
           [
             ( Nolabel,
               Exp.construct (mknoloc (Longident.parse "Not_found")) None );
           ])
    else if Ppx_config.native () then
      let open Ast_helper in
      Exp.case (Pat.any ())
        (Exp.variant "FutureAddedValue"
           (Some
              [%expr
                [%e
                  Exp.ident
                    {
                      Location.txt = Longident.parse "value";
                      loc = Location.none;
                    }]]))
    else
      let open Ast_helper in
      Exp.case (Pat.any ())
        (Exp.variant "FutureAddedValue"
           (Some
              [%expr
                (Obj.magic
                   [%e
                     Exp.ident
                       {
                         Location.txt = Longident.parse "value";
                         loc = Location.none;
                       }]
                  : Js.Json.t)]))
  in
  let typename_matcher =
    let open Ast_helper in
    Exp.match_ [%expr typename]
      (List.concat [ fragment_cases; [ fallback_case ] ])
  in
  if config.native then
    [%expr
      let typename =
        (value
         |> Yojson.Basic.Util.member "__typename"
         |> Yojson.Basic.Util.to_string
          : string)
      in
      ([%e typename_matcher] : [%t base_type_name (generate_type_name path)])]
    [@metaloc loc]
  else
    [%expr
      let typename =
        (Obj.magic (Js.Dict.unsafeGet (Obj.magic value) "__typename") : string)
      in
      ([%e typename_matcher] : [%t base_type_name (generate_type_name path)])]
    [@metaloc loc]

and generate_parser config (path : string list) definition = function
  | Res_nullable { loc; inner } ->
      generate_nullable_decoder config (conv_loc loc) inner path definition
  | Res_array { loc; inner } ->
      generate_array_decoder config (conv_loc loc) inner path definition
  | Res_id { loc } -> (
      let loc = conv_loc loc in
      match config.native with
      | true -> [%expr Yojson.Basic.Util.to_string value] [@metaloc loc]
      | false -> [%expr value] [@metaloc loc])
  | Res_string { loc } -> (
      let loc = conv_loc loc in
      match config.native with
      | true -> [%expr Yojson.Basic.Util.to_string value] [@metaloc loc]
      | false -> [%expr value] [@metaloc loc])
  | Res_int { loc } -> (
      let loc = conv_loc loc in
      match config.native with
      | true -> [%expr Yojson.Basic.Util.to_int value] [@metaloc loc]
      | false -> [%expr value] [@metaloc loc])
  | Res_float { loc } -> (
      let loc = conv_loc loc in
      match config.native with
      | true -> [%expr Yojson.Basic.Util.to_float value] [@metaloc loc]
      | false -> [%expr value] [@metaloc loc])
  | Res_boolean { loc } -> (
      let loc = conv_loc loc in
      match config.native with
      | true -> [%expr Yojson.Basic.Util.to_bool value] [@metaloc loc]
      | false -> [%expr value] [@metaloc loc])
  | Res_raw_scalar { loc } ->
      let loc = conv_loc loc in
      ([%expr value] [@metaloc loc])
  | Res_poly_enum { loc; enum_meta; omit_future_value } ->
      generate_poly_enum_decoder (conv_loc loc) enum_meta omit_future_value
  | Res_custom_decoder { loc; ident; inner } ->
      generate_custom_decoder config (conv_loc loc) ident inner path definition
  | Res_record
      { loc; name; fields; type_name = existing_record; interface_fragments } ->
      generate_object_decoder ~config ~loc:(conv_loc loc) ~name ~path
        ~definition ~existing_record ~interface_fragments fields
  | Res_object
      { loc; name; fields; type_name = existing_record; interface_fragments } ->
      generate_object_decoder ~config ~loc:(conv_loc loc) ~name ~path
        ~definition ~existing_record ~interface_fragments fields
  | Res_poly_variant_union
      { loc; name; fragments; exhaustive; omit_future_value } ->
      generate_poly_variant_union_decoder config (conv_loc loc) name fragments
        exhaustive omit_future_value path definition
  | Res_poly_variant_selection_set { loc; name; fragments = fields } ->
      generate_poly_variant_selection_set_decoder config (conv_loc loc) name
        fields path definition
  | Res_poly_variant_interface { loc; name; fragments } ->
      generate_poly_variant_interface_decoder config (conv_loc loc) name
        fragments (name :: path) definition
  | Res_solo_fragment_spread { loc; name; arguments } ->
      generate_solo_fragment_spread_decoder config loc name arguments definition
  | Res_error { loc; message } -> generate_error loc message
