open Graphql_compiler
open Result_structure
open Extract_type_definitions
open Source_pos
open Output_utils
open Ppxlib

let loc = Location.none

let make_error_raiser message =
  if Ppx_config.native () then
    if Ppx_config.verbose_error_handling () then
      [%expr raise (Failure ("graphql-ppx: " ^ [%e message]))]
    else [%expr raise (Failure "Unexpected GraphQL query response")]
  else if Ppx_config.verbose_error_handling () then
    [%expr JsError.throwWithMessage ("graphql-ppx: " ^ [%e message])]
  else [%expr JsError.throwWithMessage "Unexpected GraphQL query response"]

let const_str_expr s = Ast_helper.Exp.constant (Pconst_string (s, loc, None))

let rec generate_type ?atLoc ~config ~path ~raw = function
  | Res_string _ -> base_type ?loc:atLoc "string"
  | Res_nullable { inner } ->
    if raw then
      base_type ~inner:[ generate_type ~config ~path ~raw inner ] "Nullable.t"
    else
      base_type ?loc:atLoc
        ~inner:[ generate_type ~config ~path ~raw inner ]
        "option"
  | Res_array { inner } ->
    base_type ?loc:atLoc
      ~inner:[ generate_type ~config ~path ~raw inner ]
      "array"
  | Res_custom_decoder { ident = module_name; inner } ->
    if raw then generate_type ~config ~path ~raw inner
    else base_type ?loc:atLoc (module_name ^ ".t")
  | Res_id _ -> base_type ?loc:atLoc "string"
  | Res_int _ -> base_type ?loc:atLoc "int"
  | Res_float _ -> base_type ?loc:atLoc "float"
  | Res_boolean _ -> base_type ?loc:atLoc "bool"
  | Res_raw_scalar _ ->
    base_type ?loc:atLoc
      (match Ppx_config.native () with
      | true -> "Graphql_ppx_runtime.Json.t"
      | false -> "JSON.t")
  | Res_object { type_name } | Res_record { type_name } -> (
    match (type_name, raw) with
    | Some type_name, false -> base_type ?loc:atLoc type_name
    | _, _ -> base_type ?loc:atLoc (generate_type_name path))
  | Res_poly_variant_selection_set _ | Res_poly_variant_union _ ->
    base_type ?loc:atLoc (generate_type_name path)
  | Res_poly_variant_interface { name } ->
    base_type ?loc:atLoc (generate_type_name (name :: path))
  | Res_solo_fragment_spread { loc; name = module_name; type_name } ->
    if raw then
      base_type ~loc:(conv_loc loc) (module_name ^ ".Raw.t_" ^ type_name)
    else base_type ~loc:(conv_loc loc) (module_name ^ ".t_" ^ type_name)
  | Res_error { loc; message = error } ->
    Location.raise_errorf ~loc:(conv_loc loc) "%s" error
  | Res_poly_enum _ -> base_type ?loc:atLoc (generate_type_name path)

let wrap_type_declaration ?loc:_ ?manifest inner path =
  Ast_helper.Type.mk ~kind:inner ?manifest
    { loc = Location.none; txt = generate_type_name path }

let generate_opaque path (_loc : ast_location option) =
  Ast_helper.Type.mk { loc = Location.none; txt = generate_type_name path }

let raw_opaque_object interface_fragments fields =
  let has_fragments =
    fields |> List.exists (function Fragment _ -> true | _ -> false)
  in
  match (has_fragments, interface_fragments) with
  | true, _ -> true
  | _, Some (_, []) -> false
  | _, Some (_, _) -> true
  | _ -> false

let already_has__typename fields =
  List.exists
    (function Field { path = "__typename" :: _ } -> true | _ -> false)
    fields

let variant_interface_type ~name:interface_name ~config ~fragments ~path
  ~(loc : ast_location option) ~raw =
  let map_case_ty (name, res) =
    {
      prf_desc =
        Rtag
          ( {
              txt = name;
              loc =
                (match loc with
                | Some loc -> conv_loc loc
                | None -> Location.none);
            },
            false,
            [
              generate_type ~config
                ~path:(name :: interface_name :: path)
                ~raw res;
            ] );
      prf_loc =
        (match loc with Some loc -> conv_loc loc | None -> Location.none);
      prf_attributes = [];
    }
  in
  let fallback_case_ty =
    {
      prf_desc =
        Rtag
          ( {
              txt = "UnspecifiedFragment";
              loc =
                (match loc with
                | Some loc -> conv_loc loc
                | None -> Location.none);
            },
            false,
            [ base_type_name "string" ] );
      prf_loc =
        (match loc with Some loc -> conv_loc loc | None -> Location.none);
      prf_attributes = [];
    }
  in
  let fragment_case_tys = fragments |> List.map map_case_ty in
  Ast_helper.Typ.variant (fallback_case_ty :: fragment_case_tys) Closed None

let generate_variant_interface ~emit_locations ~name ~config ~fragments ~path
  ~(loc : ast_location) ~raw =
  wrap_type_declaration Ptype_abstract
    ?manifest:
      (match raw with
      | true -> None
      | false ->
        Some
          (variant_interface_type ~name ~config ~fragments ~path
             ~loc:(match emit_locations with true -> Some loc | false -> None)
             ~raw))
    ?loc:(match emit_locations with true -> Some loc | false -> None)
    (name :: path)

let generate_record_type ~config ~obj_path ~raw ~(loc : ast_location)
  ~is_variant ~interface_fragments ~emit_locations fields =
  let record_fields =
    fields
    |> List.fold_left
         (fun acc -> function
           | Fragment { key; loc_key; module_name; type_name } ->
             Ast_helper.Type.field
               { Location.txt = key; loc = Location.none }
               (Ast_helper.Typ.constr
                  {
                    Location.txt =
                      Longident.parse
                        (module_name ^ ".t"
                        ^
                        match type_name with
                        | None -> ""
                        | Some type_name -> "_" ^ type_name);
                    loc =
                      (match not raw with
                      | true -> conv_loc loc_key
                      | false -> Location.none);
                  }
                  [])
             :: acc
           | Field { path = name :: path; type_; loc_key; loc = _loc_field } ->
             let valid_name = to_valid_field_name name in
             (* When using \"Name" escape syntax, no @as attribute is needed
                because the escaped name preserves the original JSON field name.
                Only add @as for other transformations like reserved words. *)
             let needs_as_attr =
               name <> valid_name
               && not (valid_name.[0] = '\\' && valid_name.[1] = '"')
             in
             Ast_helper.Type.field
               ?loc:
                 (match emit_locations with
                 | true -> None
                 | false -> Some (conv_loc loc_key))
               ~attrs:
                 (match needs_as_attr with
                 | false -> []
                 | true ->
                   [
                     Ast_helper.Attr.mk
                       { txt = "as"; loc = Location.none }
                       (PStr [ Ast_helper.Str.eval (const_str_expr name) ]);
                   ])
               {
                 Location.txt = valid_name;
                 loc =
                   (match emit_locations with
                   | true -> Location.none
                   | false -> conv_loc loc_key);
               }
               (generate_type
                  ?atLoc:
                    (match emit_locations with
                    | true -> None
                    | false -> Some (conv_loc loc_key))
                  ~config ~path:(name :: path) ~raw type_)
             :: acc
           | Field { path = []; loc } ->
             Location.raise_errorf ~loc:(loc |> conv_loc) "No path")
         []
    |> List.rev
  in
  let record_fields =
    if raw && is_variant && not (already_has__typename fields) then
      Ast_helper.Type.field
        { Location.txt = "__typename"; loc = Location.none }
        (base_type_name "string")
      :: record_fields
    else record_fields
  in
  let record_fields =
    match interface_fragments with
    | Some (_, []) | None -> record_fields
    | Some (interface_name, _fragments) ->
      Ast_helper.Type.field
        { Location.txt = "fragment"; loc = Location.none }
        (base_type (generate_type_name (interface_name :: obj_path)))
      :: record_fields
  in
  match raw && raw_opaque_object interface_fragments fields with
  | true ->
    generate_opaque obj_path
      (match emit_locations with true -> Some loc | false -> None)
  | false ->
    wrap_type_declaration (Ptype_record record_fields)
      ~loc:(match emit_locations with true -> Some loc | false -> None)
      obj_path

let generate_variant_selection ~emit_locations config fields path
  (loc : ast_location) raw =
  if raw then
    generate_opaque path
      (match emit_locations with true -> Some loc | false -> None)
  else
    wrap_type_declaration Ptype_abstract
      ~manifest:
        (Ast_helper.Typ.variant
           (fields
           |> List.map (fun ((name : Result_structure.name), res) ->
                  {
                    prf_desc =
                      Rtag
                        ( {
                            txt = String.capitalize_ascii name.item;
                            loc = Location.none;
                          },
                          false,
                          [
                            generate_type
                              ?atLoc:
                                (match emit_locations with
                                | true ->
                                  Some
                                    (conv_loc
                                       (config.Generator_utils.map_loc name.span))
                                | false -> None)
                              ~config ~path:(name.item :: path) ~raw res;
                          ] );
                    prf_loc = Location.none;
                    prf_attributes = [];
                  }))
           Closed None)
      ?loc:(match emit_locations with true -> Some loc | false -> None)
      path

let generate_variant_union ~emit_locations config
  (fields : (Result_structure.name * Result_structure.t) list) omit_future_value
  path (loc : ast_location) raw =
  if raw then
    generate_opaque path
      (match emit_locations with true -> Some loc | false -> None)
  else
    let fallback_case_ty =
      match omit_future_value with
      | true -> []
      | false ->
        [
          {
            prf_desc =
              Rtag
                ( {
                    txt = "FutureAddedValue";
                    loc =
                      (match emit_locations with
                      | true -> conv_loc loc
                      | false -> Location.none);
                  },
                  false,
                  [
                    base_type
                      (match Ppx_config.native () with
                      | true -> "Graphql_ppx_runtime.Json.t"
                      | false -> "JSON.t");
                  ] );
            prf_loc =
              (match emit_locations with
              | true -> conv_loc loc
              | false -> Location.none);
            prf_attributes = [];
          };
        ]
    in
    let fragment_case_tys =
      fields
      |> List.map (fun (name, res) ->
             {
               prf_desc =
                 Rtag
                   ( {
                       txt = name.item;
                       loc =
                         (match emit_locations with
                         | true -> conv_loc loc
                         | false -> Location.none);
                     },
                     false,
                     [
                       generate_type
                         ?atLoc:
                           (match emit_locations with
                           | true ->
                             Some
                               (conv_loc
                                  (config.Generator_utils.map_loc name.span))
                           | false -> None)
                         ~config ~path:(name.item :: path) ~raw res;
                     ] );
               prf_loc =
                 (match emit_locations with
                 | true -> conv_loc loc
                 | false -> Location.none);
               prf_attributes = [];
             })
    in
    wrap_type_declaration Ptype_abstract
      ~manifest:
        (Ast_helper.Typ.variant
           (List.concat [ fallback_case_ty; fragment_case_tys ])
           Closed None)
      ?loc:
        (match emit_locations with
        | true -> Some (conv_loc loc)
        | false -> None)
      path

let generate_enum ~emit_locations _config fields path ~(loc : ast_location) raw
  omit_future_value =
  wrap_type_declaration Ptype_abstract
    ~manifest:
      (if raw then base_type "string"
       else
         Ast_helper.Typ.variant
           (List.append
              (match omit_future_value with
              | true -> []
              | false ->
                [
                  {
                    prf_desc =
                      Rtag
                        ( {
                            txt = "FutureAddedValue";
                            loc =
                              (match emit_locations with
                              | true -> conv_loc loc
                              | false -> Location.none);
                          },
                          false,
                          [ base_type "string" ] );
                    prf_loc =
                      (match emit_locations with
                      | true -> conv_loc loc
                      | false -> Location.none);
                    prf_attributes = [];
                  };
                ])
              (fields
              |> List.map (fun field ->
                     {
                       prf_desc =
                         Rtag
                           ( {
                               txt = to_valid_ident field;
                               loc =
                                 (match emit_locations with
                                 | true -> conv_loc loc
                                 | false -> Location.none);
                             },
                             true,
                             [] );
                       prf_loc =
                         (match emit_locations with
                         | true -> conv_loc loc
                         | false -> Location.none);
                       prf_attributes = [];
                     })))
           Closed None
         [@metaloc
           match emit_locations with
           | true -> conv_loc loc
           | false -> Location.none])
    ~loc path

let generate_object_type ~emit_locations config fields obj_path raw
  (loc : ast_location) is_variant interface_fragments =
  let object_fields =
    fields
    |> List.fold_left
         (fun acc -> function
           | Fragment { key; loc_key; module_name; type_name } ->
             {
               pof_desc =
                 Otag
                   ( { txt = key; loc = Location.none },
                     Ast_helper.Typ.constr
                       {
                         Location.txt =
                           Longident.parse
                             (module_name ^ ".t"
                             ^
                             match type_name with
                             | None -> ""
                             | Some type_name -> "_" ^ type_name);
                         loc =
                           (match not raw with
                           | true -> conv_loc loc_key
                           | false -> Location.none);
                       }
                       [] );
               pof_loc = Location.none;
               pof_attributes = [];
             }
             :: acc
           | Field { path = name :: path; loc_key; type_ } ->
             {
               pof_desc =
                 Otag
                   ( { txt = to_valid_ident name; loc = Location.none },
                     generate_type
                       ?atLoc:
                         (match emit_locations with
                         | true -> None
                         | false -> Some (conv_loc loc_key))
                       ~config ~path:(name :: path) ~raw type_ );
               pof_loc = Location.none;
               pof_attributes = [];
             }
             :: acc
           | Field { path = []; loc } ->
             Location.raise_errorf ~loc:(loc |> conv_loc) "No path")
         []
    |> List.rev
  in
  let object_fields =
    if raw && is_variant && not (already_has__typename fields) then
      {
        pof_desc =
          Otag
            ( { txt = to_valid_ident "__typename"; loc = Location.none },
              base_type_name "string" );
        pof_loc = Location.none;
        pof_attributes = [];
      }
      :: object_fields
    else object_fields
  in
  let object_fields =
    match interface_fragments with
    | Some (_, []) | None -> object_fields
    | Some (interface_name, _fragments) ->
      {
        pof_desc =
          Otag
            ( { txt = "fragment"; loc = Location.none },
              base_type (generate_type_name (interface_name :: obj_path)) );
        pof_loc = Location.none;
        pof_attributes = [];
      }
      :: object_fields
  in
  match raw && raw_opaque_object interface_fragments fields with
  | true ->
    generate_opaque obj_path
      (match emit_locations with true -> Some loc | false -> None)
  | false ->
    wrap_type_declaration
      ~manifest:
        (Ast_helper.Typ.constr
           { Location.txt = Longident.parse "Js.t"; loc = Location.none }
           [ Ast_helper.Typ.object_ object_fields Closed ])
      Ptype_abstract
      ?loc:(match emit_locations with true -> Some loc | false -> None)
      obj_path

let generate_graphql_object ~(config : Generator_utils.output_config) ~obj_path
  ~raw ~emit_locations ~loc ~is_variant ~type_name ~interface_fragments fields =
  match type_name with
  | Some type_name ->
    wrap_type_declaration ~manifest:(base_type type_name) Ptype_abstract
      ?loc:(match emit_locations with true -> Some loc | false -> None)
      obj_path
  | None ->
    generate_record_type ~emit_locations ~config ~obj_path ~raw ~loc ~is_variant
      ~interface_fragments fields

let generate_types ~(config : Generator_utils.output_config) ~raw
  ~emit_locations ~type_name ~fragment_name res =
  extract ~fragment_def:(Option.is_some fragment_name) ~path:[] ~raw res
  |> List.map (function
       | Object
           { fields; path = obj_path; loc; variant_parent; interface_fragments }
         ->
         generate_graphql_object ~config ~obj_path ~raw ~loc ~emit_locations
           ~is_variant:variant_parent ~type_name ~interface_fragments fields
       | VariantSelection { loc; path; fields } ->
         generate_variant_selection ~emit_locations config fields path loc raw
       | VariantUnion { loc; path; fields; omit_future_value } ->
         generate_variant_union ~emit_locations config fields omit_future_value
           path loc raw
       | VariantInterface { name; loc; path; fragments } ->
         generate_variant_interface ~emit_locations ~name ~config ~fragments
           ~path ~loc ~raw
       | Enum { loc; path; fields; omit_future_value } ->
         generate_enum ~emit_locations config fields path ~loc raw
           omit_future_value)
  |> List.rev

let make_fragment_type config raw type_name fragment_name fragment_name_loc =
  Ast_helper.Type.mk
    ~manifest:
      (Ast_helper.Typ.constr
         (match raw with
         | true -> mknoloc (Longident.Lident "t")
         | false ->
           mkloc
             (match type_name with
             | Some type_name -> Longident.parse type_name
             | None -> Longident.Lident "t")
             (match fragment_name_loc with
             | Some fragment_name_loc ->
               conv_loc (config.Generator_utils.map_loc fragment_name_loc)
             | None -> Location.none))
         [])
    (mknoloc ("t_" ^ fragment_name))

let generate_type_structure_items config res raw type_name fragment_name =
  let str_type types = Ast_helper.Str.type_ Nonrecursive types in
  let types =
    generate_types ~config ~emit_locations:false ~raw ~type_name ~fragment_name
      res
    |> List.map (fun type_ -> Ast_helper.Str.type_ Recursive [ type_ ])
  in
  match fragment_name with
  | Some (fragment_name, _fragment_name_loc) -> (
    match Schema.lookup_type config.schema fragment_name with
    | Some (Interface interface_meta) ->
      List.append types
        (str_type
           [
             make_fragment_type config raw type_name interface_meta.im_name None;
           ]
        :: (Schema.lookup_implementations config.schema interface_meta
           |> List.filter_map (fun (type_meta : Schema.type_meta) ->
                  match type_meta with
                  | Object { om_name } ->
                    Some
                      (str_type
                         [
                           make_fragment_type config raw type_name om_name None;
                         ])
                  | _ -> None)))
    | Some (Object _) ->
      List.append types
        [
          str_type
            [ make_fragment_type config raw type_name fragment_name None ];
        ]
    | _ -> types)
  | None -> types

let generate_type_signature_items (config : Generator_utils.output_config) res
  raw type_name fragment_name =
  let emit_locations = match raw with true -> false | false -> true in
  let types =
    generate_types ~config ~emit_locations ~raw ~type_name ~fragment_name res
    |> List.map (fun type_ -> Ast_helper.Sig.type_ Recursive [ type_ ])
  in
  let sig_type types = Ast_helper.Sig.type_ Nonrecursive types in
  match fragment_name with
  | Some (fragment_name, fragment_name_loc) -> (
    match Schema.lookup_type config.schema fragment_name with
    | Some (Interface interface_meta) ->
      List.append types
        (sig_type
           [
             make_fragment_type config raw type_name interface_meta.im_name None;
           ]
        :: (Schema.lookup_implementations config.schema interface_meta
           |> List.filter_map (fun (type_meta : Schema.type_meta) ->
                  match type_meta with
                  | Object { om_name } ->
                    Some
                      (sig_type
                         [
                           make_fragment_type config raw type_name om_name None;
                         ])
                  | _ -> None)))
    | Some (Object _) ->
      List.append types
        [
          sig_type
            [
              make_fragment_type config raw type_name fragment_name
                (match emit_locations with
                | true -> Some fragment_name_loc
                | false -> None);
            ];
        ]
    | _ -> types)
  | None -> types

let rec generate_arg_type ?(nulls = true) raw originalLoc =
  let loc =
    match raw with true -> None | false -> Some (conv_loc originalLoc)
  in
  function
  | Type (Scalar { sm_name = "ID" }) | Type (Scalar { sm_name = "String" }) ->
    base_type ?loc "string"
  | Type (Scalar { sm_name = "Int" }) -> base_type ?loc "int"
  | Type (Scalar { sm_name = "Float" }) -> base_type ?loc "float"
  | Type (Scalar { sm_name = "Boolean" }) -> base_type ?loc "bool"
  | Type (Scalar { sm_name = _ }) ->
    base_type ?loc
      (match Ppx_config.native () with
      | true -> "Graphql_ppx_runtime.Json.t"
      | false -> "JSON.t")
  | Type (Enum enum_meta) ->
    if raw then base_type "string"
    else
      let open Graphql_compiler.Schema in
      Ast_helper.Typ.variant ?loc
        (enum_meta.em_values
        |> List.map (fun { evm_name; _ } ->
               {
                 prf_desc =
                   Rtag ({ txt = evm_name; loc = Location.none }, true, []);
                 prf_loc = Location.none;
                 prf_attributes = [];
               }))
        Closed None
  | Type (InputObject { iom_name }) ->
    base_type ?loc (generate_type_name ~prefix:"t_variables" [ iom_name ])
  | Type (Object _) ->
    Location.raise_errorf ~loc:(originalLoc |> conv_loc)
      "Object not allowed in args"
  | Type (Union _) ->
    Location.raise_errorf ~loc:(originalLoc |> conv_loc)
      "Union not allowed in args"
  | Type (Interface _) ->
    Location.raise_errorf ~loc:(originalLoc |> conv_loc)
      "Interface not allowed in args"
  | Nullable inner -> (
    match nulls with
    | true ->
      base_type ?loc
        ~inner:[ generate_arg_type raw (conv_loc_from_ast Location.none) inner ]
        (match raw with true -> "Nullable.t" | false -> "option")
    | false -> generate_arg_type raw (conv_loc_from_ast Location.none) inner)
  | List inner ->
    base_type ?loc
      ~inner:[ generate_arg_type raw (conv_loc_from_ast Location.none) inner ]
      "array"
  | TypeNotFound name ->
    Location.raise_errorf ~loc:(originalLoc |> conv_loc) "Type %s not found!"
      name

let generate_empty_input_object impl raw =
  match Ppx_config.native () && raw with
  | true ->
    Ast_helper.Type.mk
      ?manifest:
        (match not impl with
        | true -> None
        | false -> Some (base_type_name "Graphql_ppx_runtime.Json.t"))
      { loc = Location.none; txt = generate_type_name ~prefix:"t_variables" [] }
  | false ->
    Ast_helper.Type.mk ~manifest:(base_type_name "unit")
      { loc = Location.none; txt = generate_type_name ~prefix:"t_variables" [] }

let generate_record_input_object raw input_obj_name fields =
  Ast_helper.Type.mk
    ~kind:
      (Ptype_record
         (fields
         |> List.map (function InputField { name; type_; loc } ->
              let valid_name = to_valid_ident name in
              Ast_helper.Type.field
                ~attrs:
                  (if valid_name = name then []
                   else
                     match Ppx_config.native () with
                     | true -> []
                     | false ->
                       [
                         Ast_helper.Attr.mk
                           { txt = "as"; loc = Location.none }
                           (PStr [ Ast_helper.Str.eval (const_str_expr name) ]);
                       ])
                {
                  Location.txt = valid_name;
                  loc =
                    (match raw with
                    | true -> Location.none
                    | false -> conv_loc loc);
                }
                (generate_arg_type raw
                   {
                     loc with
                     loc_ghost =
                       (match input_obj_name with
                       | None -> false
                       | Some _ -> true);
                   }
                   type_))))
    {
      loc = Location.none;
      txt =
        generate_type_name ~prefix:"t_variables"
          (match input_obj_name with None -> [] | Some name -> [ name ]);
    }

let generate_native_raw_input_object impl input_obj_name =
  Ast_helper.Type.mk
    ?kind:(match impl with true -> None | false -> Some Ptype_abstract)
    ?manifest:
      (match impl with
      | true ->
        Some
          (Ast_helper.Typ.constr
             (mknoloc (Longident.parse "Graphql_ppx_runtime.Json.t"))
             [])
      | false -> None)
    {
      loc = Location.none;
      txt =
        generate_type_name ~prefix:"t_variables"
          (match input_obj_name with None -> [] | Some name -> [ name ]);
    }

let generate_input_object impl raw _ input_obj_name fields =
  match Ppx_config.native () && raw with
  | true -> generate_native_raw_input_object impl input_obj_name
  | false -> generate_record_input_object raw input_obj_name fields

let generate_arg_type_structure_items raw config variable_defs =
  let input_objects = extract_args config variable_defs in
  [
    input_objects
    |> List.map (function
         | NoVariables -> generate_empty_input_object true raw
         | InputObject { name; fields } ->
           generate_input_object true raw config name fields)
    |> Ast_helper.Str.type_ Recursive;
  ]

let generate_arg_type_signature_items raw config variable_defs =
  let input_objects = extract_args config variable_defs in
  [
    input_objects
    |> List.map (function
         | NoVariables -> generate_empty_input_object false raw
         | InputObject { name; fields } ->
           generate_input_object false raw config name fields)
    |> Ast_helper.Sig.type_ Recursive;
  ]
