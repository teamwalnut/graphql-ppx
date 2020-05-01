open Schema;

let rec print_type_ref = (type_ref: type_ref) => {
  switch (type_ref) {
  | Schema.Named(n) => n
  | List(t) => "[" ++ print_type_ref(t) ++ "]"
  | NonNull(t) => print_type_ref(t) ++ "!"
  };
};

let print_field = ({fm_name, fm_field_type}: field_meta) => {
  fm_name ++ ": " ++ (fm_field_type |> print_type_ref);
};

let print_input_field = ({am_name, am_arg_type}: argument_meta) => {
  am_name ++ ": " ++ (am_arg_type |> print_type_ref);
};

let print_object = ({om_fields, om_name}: object_meta) => {
  (
    om_fields
    |> List.fold_left(
         (p, {fm_name} as field: Schema.field_meta) => {
           fm_name != "__typename" ? p ++ "\n  " ++ print_field(field) : p
         },
         om_name ++ " {",
       )
  )
  ++ "\n}";
};

let print_input_object = ({iom_input_fields, iom_name}: input_object_meta) => {
  (
    iom_input_fields
    |> List.fold_left(
         (p, {am_name} as input_field: Schema.argument_meta) =>
           p ++ "\n  " ++ print_input_field(input_field),
         iom_name ++ " {",
       )
  )
  ++ "\n}";
};

let print_scalar = ({sm_name}: scalar_meta) => sm_name;

let print_type = (name: string, schema: t) => {
  Schema.lookup_type(schema, name)
  |> Option.flat_map(
       fun
       | Object(obj_meta) => Some(print_object(obj_meta))
       | Scalar(scalar_meta) => Some(print_scalar(scalar_meta))
       | InputObject(input_obj_meta) =>
         Some(print_input_object(input_obj_meta))
       | Enum(_)
       | Interface(_)
       | Union(_) => None,
     );
};
