type type_ref =
  | Named(string)
  | NonNull(type_ref)
  | List(type_ref);

type argument_meta = {
  am_name: string,
  am_description: option(string),
  am_arg_type: type_ref,
  am_default_value: option(string),
};

type field_meta = {
  fm_name: string,
  fm_description: option(string),
  fm_arguments: list(argument_meta),
  fm_field_type: type_ref,
  fm_deprecation_reason: option(string),
};

type scalar_meta = {
  sm_name: string,
  sm_description: option(string),
};

type object_meta = {
  om_name: string,
  om_description: option(string),
  om_fields: list(field_meta),
  om_interfaces: list(string),
};

type enum_value_meta = {
  evm_name: string,
  evm_description: option(string),
  evm_deprecation_reason: option(string),
};

type enum_meta = {
  em_name: string,
  em_description: option(string),
  em_values: list(enum_value_meta),
};

type interface_meta = {
  im_name: string,
  im_description: option(string),
  im_fields: list(field_meta),
};

type union_meta = {
  um_name: string,
  um_description: option(string),
  um_of_types: list(string),
};

type input_object_meta = {
  iom_name: string,
  iom_description: option(string),
  iom_input_fields: list(argument_meta),
};

type type_meta =
  | Scalar(scalar_meta)
  | Object(object_meta)
  | Enum(enum_meta)
  | Interface(interface_meta)
  | Union(union_meta)
  | InputObject(input_object_meta);

type schema_meta = {
  sm_query_type: string,
  sm_mutation_type: option(string),
  sm_subscription_type: option(string),
};

type directive_location =
  | Dl_query
  | Dl_mutation
  | Dl_subscription
  | Dl_field
  | Dl_fragment_definition
  | Dl_fragment_spread
  | Dl_inline_fragment
  | Dl_unknown;

type directive_meta = {
  // It would be great to add this to the schema,
  // but when the schema changes, there will be a breaking change
  // so the filename of the schema cache need to change as well,
  // to avoid a crash
  // dm_description: option(string),
  dm_name: string,
  dm_locations: list(directive_location),
  dm_arguments: list(argument_meta),
};

type t = {
  meta: schema_meta,
  type_map: Hashtbl.t(string, type_meta),
  directive_map: Hashtbl.t(string, directive_meta),
};

let query_type = s => Hashtbl.find(s.type_map, s.meta.sm_query_type);
let mutation_type = s =>
  switch (s.meta.sm_mutation_type) {
  | Some(n) => Some(Hashtbl.find(s.type_map, n))
  | None => None
  };
let subscription_type = s =>
  switch (s.meta.sm_subscription_type) {
  | Some(n) => Some(Hashtbl.find(s.type_map, n))
  | None => None
  };

exception Invalid_type(string);
exception Inconsistent_schema(string);

let lookup_implementations = (schema, im) => {
  let all_objects_implementing_interface = (_, value, acc) =>
    switch (value) {
    | Object({om_interfaces, _}) as o
        when List.exists(n => n == im.im_name, om_interfaces) => [
        o,
        ...acc,
      ]
    | _ => acc
    };

  Hashtbl.fold(all_objects_implementing_interface, schema.type_map, []);
};

let lookup_field = (ty, name) => {
  let find_field = fs =>
    switch (List.find_all(f => f.fm_name == name, fs)) {
    | [] => None
    | [x] => Some(x)
    | _ => raise @@ Inconsistent_schema("Multiple fields named " ++ name)
    };

  switch (ty) {
  | Object({om_fields, _}) => find_field(om_fields)
  | Interface({im_fields, _}) => find_field(im_fields)
  | Scalar({sm_name, _}) =>
    raise @@ Invalid_type("Type " ++ sm_name ++ " doesn't have any fields")
  | Enum({em_name, _}) =>
    raise @@ Invalid_type("Type " ++ em_name ++ " doesn't have any fields")
  | Union({um_name, _}) =>
    raise @@ Invalid_type("Type " ++ um_name ++ " doesn't have any fields")
  | InputObject({iom_name, _}) =>
    raise @@ Invalid_type("Type " ++ iom_name ++ " doesn't have any fields")
  };
};

let lookup_input_field = (ty, name) => {
  let find_field = fs =>
    switch (List.find_all(am => am.am_name == name, fs)) {
    | [] => None
    | [x] => Some(x)
    | _ =>
      raise @@ Inconsistent_schema("Multiple input fields named " ++ name)
    };

  switch (ty) {
  | Object({om_name: name, _})
  | Interface({im_name: name, _})
  | Scalar({sm_name: name, _})
  | Enum({em_name: name, _})
  | Union({um_name: name, _}) =>
    raise @@ Invalid_type("Type " ++ name ++ " doesn't have any input fields")
  | InputObject({iom_input_fields, _}) => find_field(iom_input_fields)
  };
};

let type_name = ty =>
  switch (ty) {
  | Scalar({sm_name, _}) => sm_name
  | Object({om_name, _}) => om_name
  | Enum({em_name, _}) => em_name
  | Interface({im_name, _}) => im_name
  | Union({um_name, _}) => um_name
  | InputObject({iom_name, _}) => iom_name
  };

let is_type_default = ty =>
  switch (ty |> type_name) {
  | "ID"
  | "String"
  | "Int"
  | "Float"
  | "Boolean" => true
  | _ => false
  };

let lookup_type = (schema, name) =>
  switch (Hashtbl.find_all(schema.type_map, name)) {
  | [] => None
  | [x] => Some(x)
  | _ => raise @@ Inconsistent_schema("Multiple types named " ++ name)
  };

let lookup_directive = (schema, name) =>
  switch (Hashtbl.find_all(schema.directive_map, name)) {
  | [] => None
  | [x] => Some(x)
  | _ => raise @@ Inconsistent_schema("Multiple directives named " ++ name)
  };

let all_enums = schema =>
  Hashtbl.fold(
    (_, v, acc) =>
      switch (v) {
      | Enum(e) => [e, ...acc]
      | _ => acc
      },
    schema.type_map,
    [],
  );

let extract_name_from_type_meta =
  fun
  | Scalar({sm_name: x, _})
  | Object({om_name: x, _})
  | Enum({em_name: x, _})
  | Interface({im_name: x, _})
  | Union({um_name: x, _})
  | InputObject({iom_name: x, _}) => x;

let compare_type_meta = (x, y) =>
  String.compare(
    extract_name_from_type_meta(x),
    extract_name_from_type_meta(y),
  );

let rec innermost_name =
  fun
  | Named(name) => name
  | NonNull(inner)
  | List(inner) => innermost_name(inner);
