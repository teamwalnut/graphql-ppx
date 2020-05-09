open Source_pos;

type type_ref =
  | Tr_named(spanning(string))
  | Tr_list(spanning(type_ref))
  | Tr_non_null_named(spanning(string))
  | Tr_non_null_list(spanning(type_ref));

type input_value =
  | Iv_null
  | Iv_int(int)
  | Iv_float(float)
  | Iv_string(string)
  | Iv_boolean(bool)
  | Iv_enum(string)
  | Iv_variable(string)
  | Iv_list(list(spanning(input_value)))
  | Iv_object(list((spanning(string), spanning(input_value))));

type variable_definition = {
  vd_type: spanning(type_ref),
  vd_default_value: option(spanning(input_value)),
};

type variable_definitions = list((spanning(string), variable_definition));

type argument = (spanning(string), spanning(input_value));
type arguments = list(argument);

type directive = {
  d_name: spanning(string),
  d_arguments: option(spanning(arguments)),
};

type fragment_spread = {
  fs_name: spanning(string),
  fs_directives: list(spanning(directive)),
};

type field = {
  fd_alias: option(spanning(string)),
  fd_name: spanning(string),
  fd_arguments: option(spanning(arguments)),
  fd_directives: list(spanning(directive)),
  fd_selection_set: option(spanning(list(selection))),
}
and inline_fragment = {
  if_type_condition: option(spanning(string)),
  if_directives: list(spanning(directive)),
  if_selection_set: spanning(list(selection)),
}
and selection =
  | Field(spanning(field))
  | FragmentSpread(spanning(fragment_spread))
  | InlineFragment(spanning(inline_fragment));

type operation_type =
  | Query
  | Mutation
  | Subscription;

type operation = {
  o_type: operation_type,
  o_name: option(spanning(string)),
  o_variable_definitions: option(spanning(variable_definitions)),
  o_directives: list(spanning(directive)),
  o_selection_set: spanning(list(selection)),
};

type fragment = {
  fg_name: spanning(string),
  fg_type_condition: spanning(string),
  fg_directives: list(spanning(directive)),
  fg_selection_set: spanning(list(selection)),
};

type definition =
  | Operation(spanning(operation))
  | Fragment(spanning(fragment));

type document = list(definition);

let rec innermost_name =
  fun
  | Tr_named({item, _})
  | Tr_non_null_named({item, _}) => item
  | Tr_list({item, _})
  | Tr_non_null_list({item, _}) => innermost_name(item);
