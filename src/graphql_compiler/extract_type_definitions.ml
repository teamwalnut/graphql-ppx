open Result_structure
open Generator_utils
open Schema
open Source_pos

type path = string list

type extracted_type =
  | Type of Schema.type_meta
  | TypeNotFound of string
  | Nullable of extracted_type
  | List of extracted_type

type object_field =
  | Field of {
      type_ : Result_structure.t;
      loc_key : Source_pos.ast_location;
      loc : Source_pos.ast_location;
      path : path;
      arguments : Graphql_ast.arguments;
    }
  | Fragment of {
      module_name : string;
      key : string;
      loc_key : Source_pos.ast_location;
      type_name : string option;
    }

and type_def =
  | Object of {
      loc : Source_pos.ast_location;
      variant_parent : bool;
      path : path;
      existing_type : string option;
      fields : object_field list;
      interface_fragments : (string * (string * Result_structure.t) list) option;
    }
  | VariantSelection of {
      loc : Source_pos.ast_location;
      path : path;
      fields : (Result_structure.name * Result_structure.t) list;
    }
  | VariantUnion of {
      loc : Source_pos.ast_location;
      path : path;
      fields : (Result_structure.name * Result_structure.t) list;
      omit_future_value : bool;
    }
  | VariantInterface of {
      name : string;
      loc : Source_pos.ast_location;
      path : path;
      fragments : (string * Result_structure.t) list;
    }
  | Enum of {
      loc : Source_pos.ast_location;
      path : path;
      fields : string list;
      omit_future_value : bool;
    }

type input_object_field =
  | InputField of {
      type_ : extracted_type;
      name : string;
      loc : Source_pos.ast_location;
      loc_type : Source_pos.ast_location option;
    }

type arg_type_def =
  | NoVariables
  | InputObject of {
      name : string option;
      fields : input_object_field list;
      loc : Source_pos.ast_location;
      is_recursive : bool;
    }

let has_required_variables (arg_types : arg_type_def list) =
  List.fold_left
    (fun has_required -> function
      | NoVariables -> has_required
      | InputObject { name = Some _ } -> has_required
      | InputObject { name = None; fields } ->
        List.fold_left
          (fun has_required -> function
            | InputField { type_ = Nullable _ } -> has_required
            | InputField { type_ = _ } -> true)
          has_required fields)
    false arg_types

let generate_type_name ?(prefix = "t") = function
  | [] -> prefix
  | path ->
    path |> List.rev |> List.fold_left (fun acc item -> acc ^ "_" ^ item) prefix

let rec extract ?(fragment_def = false) ?(variant = false) ~path ~raw = function
  | Res_nullable { inner } -> extract ~path ~raw inner
  | Res_array { inner } -> extract ~path ~raw inner
  | (Res_object { loc; fields; type_name; interface_fragments } as
    result_structure)
  | (Res_record { loc; fields; type_name; interface_fragments } as
    result_structure) -> (
    match (result_structure, type_name, raw, fragment_def) with
    | _, Some _type_name, false, false -> create_children path raw fields
    | _, Some _type_name, false, true ->
      create_object ~path ~raw ~loc ~variant_parent:variant
        ~existing_type:type_name ~interface_fragments fields
    | Res_record _, _, false, _ ->
      create_object ~path ~raw ~loc ~variant_parent:variant ~existing_type:None
        ~interface_fragments fields
    | _, _, _, _ ->
      create_object ~path ~raw ~loc ~variant_parent:variant ~existing_type:None
        ~interface_fragments fields)
  | Res_poly_variant_union { loc; fragments; omit_future_value } ->
    VariantUnion { path; fields = fragments; loc; omit_future_value }
    :: extract_fragments
         (fragments
         |> List.map (fun (({ item = name } : Result_structure.name), t) ->
              (name, t)))
         path raw
  | Res_poly_variant_selection_set { loc; fragments } ->
    VariantSelection { path; fields = fragments; loc }
    :: extract_fragments
         (fragments
         |> List.map (fun (({ item = name } : Result_structure.name), t) ->
              (name, t)))
         path raw
  | Res_poly_variant_interface { loc; name; fragments } ->
    VariantInterface { name; path; fragments; loc }
    :: extract_fragments fragments (name :: path) raw
  | Res_custom_decoder { inner } -> extract ~path ~raw inner
  | Res_solo_fragment_spread _ -> []
  | Res_error _ -> []
  | Res_id _ -> []
  | Res_string _ -> []
  | Res_int _ -> []
  | Res_float _ -> []
  | Res_boolean _ -> []
  | Res_raw_scalar _ -> []
  | Res_poly_enum { loc; enum_meta; omit_future_value } ->
    [
      Enum
        {
          path;
          fields =
            enum_meta.em_values |> List.map (fun { evm_name; _ } -> evm_name);
          loc;
          omit_future_value;
        };
    ]

and _fragment_names f = f |> List.map (fun (name, _) -> name)

and extract_fragments fragments path raw =
  fragments
  |> List.fold_left
       (fun acc (name, inner) ->
         List.append (extract ~variant:true ~path:(name :: path) ~raw inner) acc)
       []

and create_children path raw fields =
  fields
  |> List.fold_left
       (fun acc -> function
         | Fr_named_field { name; type_ } ->
           List.append (extract ~path:(name :: path) ~raw type_) acc
         | Fr_fragment_spread _ -> acc)
       []

and create_object ~path ~raw ~loc ~variant_parent ~existing_type
  ~interface_fragments fields =
  let object_fields =
    fields
    |> List.map (function
         | Fr_named_field { name; loc; loc_key; type_; arguments } ->
           Field { loc; loc_key; path = name :: path; type_; arguments }
         | Fr_fragment_spread { key; loc = loc_key; name; type_name } ->
           Fragment { module_name = name; key; loc_key; type_name })
  in
  [
    [
      Object
        {
          variant_parent;
          loc;
          path;
          existing_type;
          fields = object_fields;
          interface_fragments;
        };
    ];
    create_children path raw fields;
    (match interface_fragments with
    | Some (_, []) -> []
    | Some (name, fragments) ->
      VariantInterface { name; loc; path; fragments }
      :: extract_fragments fragments (name :: path) raw
    | None -> []);
  ]
  |> List.concat

let _raise_inconsistent_schema type_name loc =
  raise_error_with_loc loc
    ("Inconsistent schema, type named " ^ type_name ^ " cannot be found")

let fetch_type schema type_ref =
  let type_name = innermost_name type_ref in
  (type_name, lookup_type schema type_name)

let rec convert_type_ref schema = function
  | Named type_name ->
    Nullable
      (match lookup_type schema type_name with
      | None -> TypeNotFound type_name
      | Some type_ -> Type type_)
  | NonNull (Named type_name) -> (
    match lookup_type schema type_name with
    | None -> TypeNotFound type_name
    | Some type_ -> Type type_)
  | List type_ref -> Nullable (List (convert_type_ref schema type_ref))
  | NonNull (List type_ref) -> List (convert_type_ref schema type_ref)
  | NonNull (NonNull inner) -> convert_type_ref schema (NonNull inner)

let generate_input_field_types _input_obj_name (schema : Schema.t)
  (fields :
    (string
    * Schema.type_ref
    * Source_pos.ast_location
    * Source_pos.ast_location option)
    list) =
  fields
  |> List.fold_left
       (fun acc (name, type_ref, loc, loc_type) ->
         InputField
           { name; type_ = convert_type_ref schema type_ref; loc; loc_type }
         :: acc)
       []
  |> List.rev

let rec get_inner_type (type_ : extracted_type) =
  match type_ with
  | Type _ -> Some type_
  | Nullable inner -> get_inner_type inner
  | List inner -> get_inner_type inner
  | TypeNotFound _ -> None

let get_input_object_name (InputField { type_; _ }) =
  let type_ = get_inner_type type_ in
  match type_ with
  | Some (Type (InputObject { iom_name; _ })) -> Some iom_name
  | _ -> None

let get_input_object_names (fields : input_object_field list) =
  fields
  |> List.map get_input_object_name
  |> List.fold_left
       (fun acc -> function Some name -> name :: acc | _ -> acc)
       []

let rec extract_input_object (schema : Schema.t) finalized_input_objects
  ( (name : string option),
    (fields : (string * Schema.type_ref * loc * loc option) list),
    loc ) =
  let gen_fields = generate_input_field_types name schema fields in
  let is_recursive =
    match name with
    | None -> false
    | Some name ->
      gen_fields |> get_input_object_names |> List.exists (fun f -> f = name)
  in
  InputObject { name; fields = gen_fields; loc; is_recursive }
  :: (fields
     |> List.fold_left
          (fun acc (_name, type_ref, loc, _) ->
            let _type_name, type_ = fetch_type schema type_ref in
            match type_ with
            | Some (Schema.InputObject { iom_name; iom_input_fields; _ }) ->
              let finalized_input_objects_in_same_list =
                acc
                |> List.fold_left
                     (fun acc -> function
                       | InputObject { name = Some name } -> name :: acc
                       | _ -> acc)
                     []
              in
              let finalized_input_objects =
                List.append finalized_input_objects
                  finalized_input_objects_in_same_list
              in
              let already_created_earlier =
                finalized_input_objects |> List.exists (fun f -> f = iom_name)
              in
              if already_created_earlier then acc
              else
                let fields =
                  iom_input_fields
                  |> List.map (fun field ->
                       (field.am_name, field.am_arg_type, loc, None))
                in
                let result =
                  extract_input_object schema
                    (iom_name :: finalized_input_objects)
                    (Some iom_name, fields, loc)
                in
                List.append acc result
            | _ -> acc)
          [])

let extract_args config args : arg_type_def list =
  match args with
  | Some { item; span } ->
    ( None,
      item
      |> List.map
           (fun
             ({ item = name; span }, { Graphql_ast.vd_type = variable_type; _ })
           ->
           ( name,
             Type_utils.to_schema_type_ref variable_type.item,
             config.map_loc span,
             Some (config.map_loc variable_type.span) )),
      config.map_loc span )
    |> extract_input_object config.schema []
  | _ -> [ NoVariables ]
