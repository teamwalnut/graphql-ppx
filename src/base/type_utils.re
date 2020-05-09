open Graphql_ast;
open Source_pos;
open Schema;

type native_type_ref =
  | Ntr_named(string)
  | Ntr_nullable(native_type_ref)
  | Ntr_list(native_type_ref);

let rec to_native_type_ref = tr =>
  switch (tr) {
  | NonNull(Named(n)) => Ntr_named(n)
  | NonNull(List(l)) => Ntr_list(to_native_type_ref(l))
  | NonNull(i) => to_native_type_ref(i)
  | List(l) => Ntr_nullable(Ntr_list(to_native_type_ref(l)))
  | Named(n) => Ntr_nullable(Ntr_named(n))
  };

let rec to_schema_type_ref = tr =>
  switch (tr) {
  | Tr_list(l) => List(to_schema_type_ref(l.item))
  | Tr_named(n) => Named(n.item)
  | Tr_non_null_list(l) => NonNull(List(to_schema_type_ref(l.item)))
  | Tr_non_null_named(n) => NonNull(Named(n.item))
  };

let is_nullable =
  fun
  | Ntr_named(_)
  | Ntr_list(_) => false
  | Ntr_nullable(_) => true;

module Generic = {
  type t =
    | ID
    | String
    | Int
    | Float
    | Boolean
    | Null
    | CustomScalar
    | Object
    | InputObject
    | Enum
    | Interface
    | Union
    | EmptyList;

  type tree =
    | T(t)
    | NonNull(tree)
    | List(tree);

  let string_of_t =
    fun
    | ID => "ID"
    | String => "String"
    | Int => "Int"
    | Float => "Float"
    | Boolean => "Boolean"
    | Object => "Object"
    | InputObject => "InputObject"
    | Enum => "Enum"
    | Interface => "Interface"
    | Union => "Union"
    | Null => "Null"
    | CustomScalar => "CustomScalar"
    | EmptyList => "EmptyList";

  let rec string_of_tree =
    fun
    | T(t) => t |> string_of_t
    | NonNull(List(t)) => "[" ++ string_of_tree(t) ++ "]!"
    | NonNull(t) => string_of_tree(t) ++ "!"
    | List(t) => "[" ++ string_of_tree(t) ++ "]";

  let eq = (t1: t, t2: t) =>
    switch (t1, t2) {
    | (ID, String)
    | (String, ID)
    | (Object, InputObject)
    | (InputObject, Object) => true
    | (t1, t2) when t1 === t2 => true
    | _ => false
    };
  let eq_tree = (t1: tree, t2: tree) =>
    t1 |> string_of_tree === (t2 |> string_of_tree);

  type apply_result =
    | Ok
    | Unequal
    | RequiredMismatch;
  let rec can_apply = (to_: tree, from: tree) => {
    switch (to_, from) {
    // Disable checks for custom scalars
    | (T(CustomScalar), T(_)) => Ok
    | (T(CustomScalar), NonNull(_)) => Ok
    // Needs recursive check
    | (NonNull(t1), NonNull(t2))
    | (List(t1), List(t2))
    | (List(t1), NonNull(List(t2))) => can_apply(t1, t2)
    // Normal type equality
    | (T(t1), T(t2))
    | (T(t1), NonNull(T(t2))) => eq(t1, t2) ? Ok : Unequal
    // Default value "EmptyList" is acceptable for list types
    | (List(_), T(EmptyList))
    | (List(_), NonNull(T(EmptyList)))
    | (NonNull(List(_)), T(EmptyList)) => Ok
    // Left side is required, right side is not
    | (NonNull(_), T(_))
    | (NonNull(_), List(_)) => RequiredMismatch
    | _ => Unequal
    };
  };

  let from_schema_tm = (ty: Schema.type_meta) =>
    switch (ty) {
    | Scalar({sm_name: "ID"}) => ID
    | Scalar({sm_name: "String"}) => String
    | Scalar({sm_name: "Int"}) => Int
    | Scalar({sm_name: "Float"}) => Float
    | Scalar({sm_name: "Boolean"}) => Boolean
    | Scalar(_) => CustomScalar
    | Object(_) => Object
    | InputObject(_) => InputObject
    | Enum(_) => Enum
    | Interface(_) => Interface
    | Union(_) => Union
    };
  let rec from_schema_tr = (~schema: Schema.t, tr: Schema.type_ref) => {
    switch (tr) {
    | Named(name) =>
      T(
        name
        |> Schema.lookup_type(schema)
        |> Option.map(from_schema_tm)
        |> Option.get_or_else(Null),
      )
    | NonNull(tr) => NonNull(tr |> from_schema_tr(~schema))
    | List(tr) => List(tr |> from_schema_tr(~schema))
    };
  };

  let rec from_graphql_ast_tr = (~schema: Schema.t, tr: Graphql_ast.type_ref) => {
    switch (tr) {
    | Tr_named(name) =>
      T(
        name.item
        |> Schema.lookup_type(schema)
        |> Option.map(from_schema_tm)
        |> Option.get_or_else(Null),
      )
    | Tr_non_null_named(name) =>
      NonNull(Tr_named(name) |> from_graphql_ast_tr(~schema))
    | Tr_non_null_list(name) =>
      NonNull(Tr_list(name) |> from_graphql_ast_tr(~schema))
    | Tr_list(t) => List(t.item |> from_graphql_ast_tr(~schema))
    };
  };

  let rec from_graphql_ast_iv =
          (
            ~arguments: Hashtbl.t(string, tree)=Hashtbl.create(0),
            ~schema: Schema.t,
            iv: Graphql_ast.input_value,
          ) =>
    switch (iv) {
    | Iv_null => NonNull(T(Null))
    | Iv_int(_) => NonNull(T(Int))
    | Iv_float(_) => NonNull(T(Float))
    | Iv_string(_) => NonNull(T(String))
    | Iv_boolean(_) => NonNull(T(Boolean))
    | Iv_enum(_) => NonNull(T(Enum))
    | Iv_variable(variable_name) =>
      try(Hashtbl.find(arguments, variable_name)) {
      | Not_found => T(Null)
      }
    | Iv_list([inner, ..._]) =>
      NonNull(List(inner.item |> from_graphql_ast_iv(~arguments, ~schema)))
    | Iv_list([]) => NonNull(T(EmptyList))
    | Iv_object(_) => NonNull(T(InputObject))
    };

  type error =
    | MismatchedTypes(string, string)
    | MismatchedRequiredVar(string, string)
    | RequiredFieldMissing(string, string)
    | RequiredVariableMissing(string, string);
  let generate_error = (error: error) => {
    switch (error) {
    | MismatchedTypes(expected, received) =>
      Printf.sprintf(
        "Invalid argument. Expected \"%s\" but received \"%s\".",
        expected,
        received,
      )
    | MismatchedRequiredVar(arg_name, val_name) =>
      Printf.sprintf(
        "Invalid argument. Trying to apply the nullable variable \"$%s\" to the required argument \"%s\".",
        val_name,
        arg_name,
      )
    | RequiredFieldMissing(arg_name, key_name) =>
      Printf.sprintf(
        "Invalid argument. The field \"%s\" on argument \"%s\" is missing.",
        key_name,
        arg_name,
      )
    | RequiredVariableMissing(arg_name, variable_name) =>
      Printf.sprintf(
        "Invalid argument. The variable \"$%s\" on argument \"%s\" is missing.",
        variable_name,
        arg_name,
      )
    };
  };
};
