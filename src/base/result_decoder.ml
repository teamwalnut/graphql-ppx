open Graphql_ast
open Source_pos
open Schema
open Type_utils
open Generator_utils
open Result_structure

let get_with_default value default_value =
  match value with Some value -> Some value | None -> default_value

let make_error error_marker map_loc span message =
  let () = error_marker.has_error <- true in
  Res_error { loc = map_loc span; message }

let has_directive ?(prepend = false) name directives =
  List.exists
    (fun { item = { d_name = { item; _ }; _ }; _ } ->
      match prepend with
      | true -> item = "bs" ^ name || item = "ppx" ^ name
      | false -> item = name)
    directives

let find_directive ?(prepend = false) name directives =
  match
    List.find
      (fun { item = { d_name = { item; _ }; _ }; _ } ->
        match prepend with
        | true -> item = "bs" ^ name || item = "ppx" ^ name
        | false -> item = name)
      directives
  with
  | d -> Some d
  | exception Not_found -> None

let find_argument name arguments =
  arguments
  |> Option.flat_map (fun { item = arguments; _ } ->
       match
         List.find
           (fun ({ item = arg_name; _ }, _) -> arg_name = name)
           arguments
       with
       | a -> Some a
       | exception Not_found -> None)

let find_fragment_arguments
  (directives : Graphql_ast.directive Source_pos.spanning list) =
  match directives |> List.find (fun d -> d.item.d_name.item = "arguments") with
  | { item = { d_arguments = Some arguments; _ }; _ } ->
    arguments.item
    |> List.fold_left
         (fun acc -> function
           | { item = name; _ }, { item = Iv_variable variable_name; _ }
             when name = variable_name ->
             name :: acc
           | _ -> acc)
         []
  | _ -> []
  | exception Not_found -> []

let get_ppx_as directives =
  match directives |> find_directive ~prepend:true "As" with
  | None -> None
  | Some { item = { d_arguments; _ }; _ } -> (
    match find_argument "type" d_arguments with
    | Some (_, { item = Iv_string type_name; _ }) -> Some type_name
    | _ -> None)

let rec unify_type ~has_decoder error_marker as_record existing_record
  omit_future_value config span ty
  (selection_set : selection list spanning option) =
  match ty with
  | Ntr_nullable t ->
    Res_nullable
      {
        loc = config.map_loc span;
        inner =
          unify_type ~has_decoder error_marker as_record existing_record
            omit_future_value config span t selection_set;
      }
  | Ntr_list t ->
    Res_array
      {
        loc = config.map_loc span;
        inner =
          unify_type ~has_decoder error_marker as_record existing_record
            omit_future_value config span t selection_set;
      }
  | Ntr_named n -> (
    match lookup_type config.schema n with
    | None -> raise_error config.map_loc span ("Could not find type " ^ n)
    | Some (Scalar { sm_name = "ID"; _ })
    | Some (Scalar { sm_name = "String"; _ }) ->
      Res_string { loc = config.map_loc span }
    | Some (Scalar { sm_name = "Int"; _ }) ->
      Res_int { loc = config.map_loc span }
    | Some (Scalar { sm_name = "Float"; _ }) ->
      Res_float { loc = config.map_loc span }
    | Some (Scalar { sm_name = "Boolean"; _ }) ->
      Res_boolean { loc = config.map_loc span }
    | Some (Scalar _) when has_decoder ->
      Res_raw_scalar { loc = config.map_loc span }
    | Some (Scalar { sm_name }) -> (
      try
        let decoderModule =
          Hashtbl.find (Ppx_config.custom_fields ()) sm_name
        in
        Res_custom_decoder
          {
            loc = config.map_loc span;
            ident = decoderModule;
            inner = Res_raw_scalar { loc = config.map_loc span };
          }
      with
      | Not_found -> Res_raw_scalar { loc = config.map_loc span }
      | other -> raise other)
    | Some (Object _ as ty) ->
      unify_selection_set error_marker as_record existing_record config span ty
        selection_set
    | Some (Enum enum_meta) ->
      Res_poly_enum { loc = config.map_loc span; enum_meta; omit_future_value }
    | Some (Interface im as ty) ->
      unify_interface error_marker as_record config span im ty selection_set
        existing_record
    | Some (InputObject _) ->
      make_error error_marker config.map_loc span
        "Can't have fields on input objects"
    | Some (Union um) ->
      unify_union error_marker config span um omit_future_value selection_set)

and unify_interface error_marker _as_record config span interface_meta ty
  selection_set existing_record =
  match selection_set with
  | None ->
    make_error error_marker config.map_loc span
      "Interface types must have subselections"
  | Some selection_set -> (
    let unwrap_type_conds (selections, fragments) selection =
      match selection with
      | InlineFragment { item = { if_type_condition = None; _ }; span } ->
        raise_error config.map_loc span
          "Inline fragments must have a type condition"
      | InlineFragment frag -> (selections, frag :: fragments)
      | selection -> (selection :: selections, fragments)
    in
    let base_selection_set, fragments =
      List.fold_left unwrap_type_conds ([], []) selection_set.item
    in
    let generate_case selection ty name =
      ( name,
        Res_object
          {
            loc = config.map_loc span;
            name;
            fields =
              List.map
                (unify_selection ~base_type:interface_meta.im_name error_marker
                   config ty)
                selection;
            type_name = None;
            interface_fragments = None;
          } )
    in
    let generate_fragment_case
      { item = { if_type_condition; if_selection_set; _ }; _ } =
      match if_type_condition with
      | Some if_type_condition ->
        let { item; _ } = if_selection_set in
        let selection = item in
        let ty =
          match lookup_type config.schema if_type_condition.item with
          | Some ty -> ty
          | None -> ty
        in
        generate_case selection ty if_type_condition.item
      | None -> assert false
    in
    let fragment_cases = List.map generate_fragment_case fragments in
    let interface =
      Res_poly_variant_interface
        {
          loc = config.map_loc span;
          name = interface_meta.im_name;
          fragments = fragment_cases;
        }
    in
    match base_selection_set with
    | [] -> interface
    | _ ->
      Res_object
        {
          loc = config.map_loc span;
          name = interface_meta.im_name;
          fields =
            List.map
              (unify_selection ~base_type:interface_meta.im_name error_marker
                 config ty)
              base_selection_set;
          type_name = existing_record;
          interface_fragments = Some (interface_meta.im_name, fragment_cases);
        })

and unify_union error_marker config span union_meta omit_future_value
  selection_set =
  match selection_set with
  | None ->
    make_error error_marker config.map_loc span
      "Union types must have subselections"
  | Some selection_set ->
    let unwrap_type_conds selection =
      match selection with
      | Field { span; _ } | FragmentSpread { span; _ } ->
        raise_error config.map_loc span
          "Only inline fragments can appear on unions"
      | InlineFragment { item = { if_type_condition = None; _ }; span } ->
        raise_error config.map_loc span
          "Inline fragments must have a type condition"
      | InlineFragment frag -> frag
    in
    let type_cond_name { item = { if_type_condition; _ }; _ } =
      match if_type_condition with
      | Some { item; _ } -> item
      | None -> assert false
    in
    let generate_case
      { item = { if_type_condition; if_selection_set; if_directives }; _ } =
      match if_type_condition with
      | Some if_type_condition ->
        let type_cond_ty =
          match lookup_type config.schema if_type_condition.item with
          | None ->
            raise_error config.map_loc if_type_condition.span
              "Could not find type"
          | Some ty -> ty
        in
        let is_record = has_directive ~prepend:true "Record" if_directives in
        let existing_record = get_ppx_as if_directives in
        let result_decoder =
          unify_selection_set error_marker is_record existing_record config
            if_selection_set.span type_cond_ty (Some if_selection_set)
        in
        (if_type_condition, result_decoder)
      | None -> assert false
    in
    let fragments = List.map unwrap_type_conds selection_set.item in
    let covered_cases =
      List.map type_cond_name fragments |> List.sort compare
    in
    let possible_cases = List.sort compare union_meta.um_of_types in
    Res_poly_variant_union
      {
        loc = config.map_loc span;
        name = union_meta.um_name;
        fragments = List.map generate_case fragments;
        exhaustive =
          (if covered_cases = possible_cases then Exhaustive else Nonexhaustive);
        omit_future_value;
      }

and unify_variant error_marker config span ty selection_set =
  match ty with
  | Ntr_nullable t ->
    Res_nullable
      {
        loc = config.map_loc span;
        inner = unify_variant error_marker config span t selection_set;
      }
  | Ntr_list t ->
    Res_array
      {
        loc = config.map_loc span;
        inner = unify_variant error_marker config span t selection_set;
      }
  | Ntr_named n -> (
    match lookup_type config.schema n with
    | None ->
      make_error error_marker config.map_loc span ("Could not find type " ^ n)
    | Some (Scalar _)
    | Some (Enum _)
    | Some (Interface _)
    | Some (Union _)
    | Some (InputObject _) ->
      make_error error_marker config.map_loc span
        "Variant fields can only be applied to object types"
    | Some (Object _ as ty) -> (
      match selection_set with
      | None ->
        make_error error_marker config.map_loc span
          "Variant fields need a selection set"
      | Some { item; _ } ->
        let fields =
          item
          |> List.map (fun selection ->
               match selection with
               | Field { item; _ } -> (
                 match lookup_field ty item.fd_name.item with
                 | None ->
                   raise_error config.map_loc span
                     ("Unknown field on type " ^ type_name ty)
                 | Some field_meta ->
                   let key = some_or item.fd_alias item.fd_name in
                   let inner_type =
                     match to_native_type_ref field_meta.fm_field_type with
                     | Ntr_list _ | Ntr_named _ ->
                       raise_error config.map_loc span
                         "Variant field must only contain nullable fields"
                     | Ntr_nullable i -> i
                   in
                   ( key,
                     unify_type ~has_decoder:false error_marker false None false
                       config span inner_type item.fd_selection_set ))
               | FragmentSpread { span; _ } ->
                 raise_error config.map_loc span
                   "Variant selections can only contain fields"
               | InlineFragment { span; _ } ->
                 raise_error config.map_loc span
                   "Variant selections can only contain fields")
        in
        Res_poly_variant_selection_set
          { loc = config.map_loc span; name = n; fragments = fields }))

and unify_field error_marker config field_span ty =
  let ast_field = field_span.item in
  let field_name = ast_field.fd_name.item in
  let field_meta = lookup_field ty field_name in
  let key = some_or ast_field.fd_alias ast_field.fd_name in
  let key_span = key.span in
  let key = key.item in
  let is_variant =
    has_directive ~prepend:true "Variant" ast_field.fd_directives
  in
  let is_record =
    has_directive ~prepend:true "Record" ast_field.fd_directives
  in
  let omit_future_value =
    has_directive ~prepend:true "OmitFutureValue" ast_field.fd_directives
    || not config.future_added_value
  in
  let has_decoder =
    match
      ( has_directive ~prepend:true "Decoder" ast_field.fd_directives,
        has_directive ~prepend:true "Custom" ast_field.fd_directives,
        has_directive ~prepend:true "CustomOpt" ast_field.fd_directives )
    with
    | true, _, _ | _, true, _ | _, _, true -> true
    | false, false, false -> false
  in
  let existing_record = get_ppx_as ast_field.fd_directives in
  let has_skip =
    has_directive "skip" ast_field.fd_directives
    || has_directive "include" ast_field.fd_directives
  in
  let sub_unifier =
    if is_variant then unify_variant error_marker
    else
      unify_type ~has_decoder error_marker is_record existing_record
        omit_future_value
  in
  let parser_expr =
    match field_meta with
    | None ->
      make_error error_marker config.map_loc ast_field.fd_name.span
        ("Unknown field '" ^ field_name ^ "' on type " ^ type_name ty)
    | Some field_meta ->
      let field_ty = to_native_type_ref field_meta.fm_field_type in
      let sub_unifier =
        sub_unifier config field_span.span field_ty ast_field.fd_selection_set
      in
      if has_skip && not (is_nullable field_ty) then
        Res_nullable
          { loc = config.map_loc field_span.span; inner = sub_unifier }
      else sub_unifier
  in
  let loc = config.map_loc field_span.span in
  let loc_key = config.map_loc key_span in
  let arguments =
    match ast_field.fd_arguments with None -> [] | Some { item } -> item
  in
  match
    match
      ( ast_field.fd_directives |> find_directive ~prepend:true "Decoder",
        ast_field.fd_directives |> find_directive ~prepend:true "Custom",
        ast_field.fd_directives |> find_directive ~prepend:true "CustomOpt" )
    with
    | Some decoder, _, _ -> `Custom decoder
    | _, Some decoder, _ -> `Custom decoder
    | _, _, Some decoder -> `CustomOpt decoder
    | None, None, None -> `None
  with
  | `None ->
    Fr_named_field { name = key; loc_key; loc; type_ = parser_expr; arguments }
  | (`Custom { item = { d_arguments; _ }; span } as result)
  | (`CustomOpt { item = { d_arguments; _ }; span } as result) -> (
    match find_argument "module" d_arguments with
    | None ->
      Fr_named_field
        {
          name = key;
          loc;
          loc_key;
          type_ =
            make_error error_marker config.map_loc span
              "ppxDecoder must be given 'module' argument";
          arguments;
        }
    | Some (_, { item = Iv_string module_name; span }) -> (
      match (result, parser_expr) with
      | `Custom _, Res_nullable { loc; inner = t } ->
        Fr_named_field
          {
            name = key;
            loc_key;
            loc;
            type_ =
              Res_nullable
                {
                  loc;
                  inner =
                    Res_custom_decoder
                      {
                        loc = config.map_loc span;
                        ident = module_name;
                        inner = t;
                      };
                };
            arguments;
          }
      | `CustomOpt _, Res_nullable { loc; inner = t } ->
        Fr_named_field
          {
            name = key;
            loc_key;
            loc;
            type_ =
              Res_custom_decoder
                {
                  loc = config.map_loc span;
                  ident = module_name;
                  inner = Res_nullable { loc; inner = t };
                };
            arguments;
          }
      | _, Res_array { loc; inner = t } ->
        Fr_named_field
          {
            name = key;
            loc_key;
            loc;
            type_ =
              Res_array
                {
                  loc;
                  inner =
                    Res_custom_decoder
                      {
                        loc = config.map_loc span;
                        ident = module_name;
                        inner = t;
                      };
                };
            arguments;
          }
      | _ ->
        Fr_named_field
          {
            name = key;
            loc_key;
            loc;
            type_ =
              Res_custom_decoder
                {
                  loc = config.map_loc span;
                  ident = module_name;
                  inner = parser_expr;
                };
            arguments;
          })
    | Some (_, { span; _ }) ->
      Fr_named_field
        {
          name = key;
          loc_key;
          loc;
          type_ =
            make_error error_marker config.map_loc span
              "The 'module' argument must be a string";
          arguments;
        })

and unify_selection ~(base_type : string) error_marker config ty selection =
  match selection with
  | Field field_span -> unify_field error_marker config field_span ty
  | FragmentSpread { item = { fs_directives; fs_name }; span } -> (
    let arguments = find_fragment_arguments fs_directives in
    match find_directive ~prepend:true "Field" fs_directives with
    | None ->
      let key =
        fs_name.item |> String.split_on_char '.' |> List.rev |> List.hd
        |> String.uncapitalize_ascii
      in
      Fr_fragment_spread
        {
          key;
          loc = config.map_loc span;
          name = fs_name.item;
          type_name = Some base_type;
          arguments;
        }
    | Some { item = { d_arguments; _ }; span } -> (
      match find_argument "name" d_arguments with
      | None ->
        raise_error config.map_loc span "ppxField must be given 'name' argument"
      | Some (_, { item = Iv_string key; span }) ->
        Fr_fragment_spread
          {
            key;
            loc = config.map_loc span;
            name = fs_name.item;
            type_name = Some base_type;
            arguments;
          }
      | Some _ ->
        raise_error config.map_loc span "The 'name' argument must be a string"))
  | InlineFragment { span; _ } ->
    raise_error config.map_loc span "Inline fragments are not yet supported"

and unify_selection_set error_marker as_record existing_record config span ty
  selection_set =
  match selection_set with
  | None ->
    make_error error_marker config.map_loc span
      "Must select subfields on objects"
  | Some
      { item = [ FragmentSpread { item = { fs_directives; fs_name }; _ } ]; _ }
    ->
    let arguments = find_fragment_arguments fs_directives in
    if as_record then
      make_error error_marker config.map_loc span
        "@ppxRecord can not be used with fragment spreads, place @ppxRecord on \
         the fragment definition instead"
    else
      Res_solo_fragment_spread
        {
          type_name = type_name ty;
          loc = config.map_loc span;
          name = fs_name.item;
          arguments;
        }
  | Some { item; _ } when as_record ->
    Res_record
      {
        loc = config.map_loc span;
        name = type_name ty;
        fields =
          List.map
            (unify_selection ~base_type:(type_name ty) error_marker config ty)
            item;
        type_name = existing_record;
        interface_fragments = None;
      }
  | Some { item; _ } ->
    Res_object
      {
        loc = config.map_loc span;
        name = type_name ty;
        fields =
          List.map
            (unify_selection ~base_type:(type_name ty) error_marker config ty)
            item;
        type_name = existing_record;
        interface_fragments = None;
      }

let unify_operation error_marker config = function
  | { item = { o_type = Query; o_selection_set; _ }; span } ->
    unify_selection_set error_marker false None config span
      (query_type config.schema) (Some o_selection_set)
  | { item = { o_type = Mutation; o_selection_set; _ }; span } -> (
    match mutation_type config.schema with
    | Some mutation_type ->
      unify_selection_set error_marker false None config span mutation_type
        (Some o_selection_set)
    | None ->
      make_error error_marker config.map_loc span
        "This schema does not contain any mutations")
  | { item = { o_type = Subscription; o_selection_set; _ }; span } -> (
    match subscription_type config.schema with
    | Some subscription_type ->
      unify_selection_set error_marker false None config span subscription_type
        (Some o_selection_set)
    | None ->
      make_error error_marker config.map_loc span
        "This schema does not contain any subscriptions")

type query_config = {
  schema : string option;
  records : bool option;
  objects : bool option;
  inline : bool option;
  template_tag : string option;
  template_tag_location : string option;
  template_tag_import : string option;
  template_tag_return_type : string option;
  tagged_template : bool option;
  template_tag_is_function : bool option;
  future_added_value : bool option;
  extend : string option;
  fragment_in_query : Ppx_config.fragment_in_query option;
  apollo_mode : bool option;
}

let get_template_tag query_config =
  match query_config.tagged_template with
  | Some false -> (None, None, None)
  | _ -> (
    match
      ( get_with_default query_config.template_tag (Ppx_config.template_tag ()),
        get_with_default query_config.template_tag_location
          (Ppx_config.template_tag_location ()),
        get_with_default query_config.template_tag_import
          (Ppx_config.template_tag_import ()) )
    with
    | Some tag, Some location, Some import ->
      (Some tag, Some location, Some import)
    | None, Some location, Some import ->
      (Some import, Some location, Some import)
    | Some tag, Some location, None -> (Some tag, Some location, Some "default")
    | None, Some location, None -> (None, Some location, Some "default")
    | Some tag, None, Some _ | Some tag, None, None -> (Some tag, None, None)
    | None, _, _ -> (None, None, None))

let get_config_arguments directives =
  let open Graphql_ast in
  directives
  |> filter_map (function
       | {
           item =
             {
               d_name = { item = "ppxConfig" };
               d_arguments = Some { item = arguments };
             };
         } ->
         Some arguments
       | _ -> None)
  |> List.concat

(** Allow configuration per definition using directives *)
let config_arguments_to_config existing_query_config directive_arguments :
  query_config =
  let open Graphql_ast in
  directive_arguments
  |> List.fold_left
       (fun config -> function
         | { item = "schema" }, { item = Iv_string value } ->
           { config with schema = Some value }
         | { item = "records" }, { item = Iv_boolean value } ->
           { config with records = Some value }
         | { item = "objects" }, { item = Iv_boolean value } ->
           { config with objects = Some value }
         | { item = "inline" }, { item = Iv_boolean value } ->
           { config with inline = Some value }
         | { item = "templateTag" }, { item = Iv_string value } ->
           { config with template_tag = Some value }
         | { item = "templateTagLocation" }, { item = Iv_string value } ->
           { config with template_tag_location = Some value }
         | { item = "templateTagImport" }, { item = Iv_string value } ->
           { config with template_tag_import = Some value }
         | { item = "templateTagReturnType" }, { item = Iv_string value } ->
           { config with template_tag_return_type = Some value }
         | { item = "taggedTemplate" }, { item = Iv_boolean value } ->
           { config with tagged_template = Some value }
         | { item = "templateTagIsFunction" }, { item = Iv_boolean value } ->
           { config with template_tag_is_function = Some value }
         | { item = "futureAddedValue" }, { item = Iv_boolean value } ->
           { config with future_added_value = Some value }
         | { item = "extend" }, { item = Iv_string value } ->
           { config with extend = Some value }
         | { item = "fragmentInQuery" }, { item = Iv_string "include" } ->
           { config with fragment_in_query = Some Include }
         | { item = "fragmentInQuery" }, { item = Iv_string "exclude" } ->
           { config with fragment_in_query = Some Exclude }
         | { item = "apollo_mode" }, { item = Iv_boolean value } ->
           { config with apollo_mode = Some value }
         | _ -> config)
       existing_query_config

let to_output_config ~json_read_fn ~map_loc ~delimiter ~document
  (definition, query_config) =
  let schema =
    Lazy.force (Read_schema.get_schema ~json_read_fn query_config.schema)
  in
  (* let () =
       Printf.eprintf "Apollo mode? query: %s %B"
         (match query_config.apollo_mode with
         | Some true -> "Some true"
         | Some false -> "Some false"
         | None -> "None")
         (Ppx_config.apollo_mode ())
     in *)
  let apollo_mode =
    match query_config.apollo_mode with
    | None -> Ppx_config.apollo_mode ()
    | Some apollo_mode -> apollo_mode
  in
  (* let () = Printf.eprintf "Really Apollo mode2?  %B\n" apollo_mode in *)
  let definitions =
    if apollo_mode then
      [ definition ] |> Ast_transforms.add_typename_to_selection_set schema
    else [ definition ]
  in
  let definitions =
    definitions |> Ast_transforms.remove_typename_from_union schema
  in
  let definition =
    match definitions with [ definition ] -> definition | _ -> definition
  in
  let template_tag = get_template_tag query_config in
  ( definition,
    {
      Generator_utils.map_loc;
      delimiter;
      full_document = document;
      inline =
        (match query_config.inline with Some value -> value | None -> false);
      future_added_value =
        (match query_config.future_added_value with
        | Some value -> value
        | None -> Ppx_config.future_added_value ());
      schema;
      template_tag;
      template_tag_return_type =
        get_with_default query_config.template_tag_return_type
          (Ppx_config.template_tag_return_type ());
      template_tag_is_function =
        get_with_default query_config.template_tag_is_function
          (Ppx_config.template_tag_is_function ());
      extend = query_config.extend;
      fragment_in_query =
        (match query_config.fragment_in_query with
        | Some value -> value
        | None -> Ppx_config.fragment_in_query ());
      native = Ppx_config.native ();
    } )

let rec generate_config ~json_read_fn ~map_loc ~delimiter ~initial_query_config
  document =
  match document with
  | (Operation { item = { o_directives = directives } } as definition) :: rest
  | (Fragment { item = { fg_directives = directives } } as definition) :: rest
    ->
    let query_config =
      directives |> get_config_arguments
      |> config_arguments_to_config initial_query_config
    in
    to_output_config ~json_read_fn ~document ~map_loc ~delimiter
      (definition, query_config)
    :: generate_config ~json_read_fn ~map_loc ~delimiter ~initial_query_config
         rest
  | [] -> []

let rec unify_document_schema document =
  let error_marker = { Generator_utils.has_error = false } in
  match document with
  | (Operation ({ item = { o_variable_definitions; _ }; _ } as op), config)
    :: rest ->
    let structure = unify_operation error_marker config op in
    ( Def_operation
        {
          variable_definitions = o_variable_definitions;
          has_error = error_marker.has_error;
          operation = op;
          inner = structure;
        },
      config )
    :: unify_document_schema rest
  | ( Fragment
        ({
           item =
             { fg_name; fg_selection_set; fg_type_condition; fg_directives };
           span;
         } as fg),
      config )
    :: rest ->
    (let open Result in
    let with_decoder =
      match
        match
          ( fg_directives |> find_directive ~prepend:true "Decoder",
            fg_directives |> find_directive ~prepend:true "Custom" )
        with
        | Some decoder, _ | _, Some decoder -> Some decoder
        | None, None -> None
      with
      | None -> Ok None
      | Some { item = { d_arguments; _ }; span } as result -> (
        match (result, find_argument "module" d_arguments) with
        | _, None ->
          Error
            (make_error error_marker config.map_loc span
               "ppxDecoder must be given 'module' argument")
        | _, Some (_, { item = Iv_string ident; span }) ->
          Ok
            (Some
               (fun structure ->
                 Res_custom_decoder
                   { loc = config.map_loc span; ident; inner = structure }))
        | _, Some (_, { span; _ }) ->
          Error
            (make_error error_marker config.map_loc span
               "The 'module' argument must be a string"))
    in
    let is_record = has_directive ~prepend:true "Record" fg_directives in
    let argumentDefinitions = fg.item.fg_variable_definitions in
    ( (match Schema.lookup_type config.schema fg_type_condition.item with
      | None ->
        Def_fragment
          {
            name = fg_name.item;
            variable_definitions = argumentDefinitions;
            has_error = true;
            fragment = fg;
            type_name = None;
            inner =
              make_error error_marker config.map_loc fg_type_condition.span
                (Printf.sprintf "Unknown type \"%s\"" fg_type_condition.item);
          }
      | Some ty -> (
        let existing_record = get_ppx_as fg_directives in
        let structure =
          unify_selection_set error_marker is_record existing_record config span
            ty (Some fg_selection_set)
        in
        let argumentDefinitions = fg.item.fg_variable_definitions in
        match with_decoder with
        | Error err ->
          Def_fragment
            {
              name = fg_name.item;
              variable_definitions = argumentDefinitions;
              has_error = true;
              fragment = fg;
              type_name = existing_record;
              inner = err;
            }
        | Ok decoder ->
          Def_fragment
            {
              name = fg_name.item;
              variable_definitions = argumentDefinitions;
              has_error = error_marker.has_error;
              fragment = fg;
              type_name = existing_record;
              inner =
                (match decoder with
                | Some decoder -> decoder structure
                | None -> structure);
            })),
      config ))
    :: unify_document_schema rest
  | [] -> []
