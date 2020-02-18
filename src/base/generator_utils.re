type error_marker = {mutable has_error: bool};

let raise_error_with_loc = Ppx_config.raise_error_with_loc;

let raise_error = (map_loc, span, message) =>
  raise_error_with_loc(map_loc(span), message);

let some_or = (o, d) =>
  switch (o) {
  | Some(v) => v
  | None => d
  };

let capitalize_ascii = Compat.capitalize_ascii;
let uncapitalize_ascii = Compat.uncapitalize_ascii;

type output_config = {
  map_loc:
    ((Source_pos.source_position, Source_pos.source_position)) =>
    Source_pos.ast_location,
  delimiter: option(string),
  schema: Schema.schema,
  full_document: Graphql_ast.document,
  records: bool,
  inline: bool,
  legacy: bool,
  definition: bool,
};

let filter_map = (f, l) => {
  let rec loop = acc =>
    fun
    | [] => List.rev(acc)
    | [head, ...tail] =>
      switch (f(head)) {
      | None => loop(acc, tail)
      | Some(v) => loop([v, ...acc], tail)
      };

  loop([], l);
};
