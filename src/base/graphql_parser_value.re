open Result;

open Source_pos;

open Graphql_parser;
open Graphql_ast;

let map_next_token = (parser, f) =>
  switch (next(parser)) {
  | Ok(item) => Ok(map(f, item))
  | e => e
  };

let replace_next_token = (parser, t) =>
  switch (next(parser)) {
  | Ok(item) => Ok(replace(item, t))
  | Error(e) => Error(e)
  };

let map_next_token_with_error = (parser, f) =>
  switch (next(parser)) {
  | Ok(item) => Error(map(f, item))
  | Error(e) => Error(e)
  };

let parse_variable_literal = parser =>
  Result_ext.(
    expect(parser, Graphql_lexer.Dollar)
    |> flat_map(({span: (start_pos, _), _}) =>
         expect_name(parser) |> map(make_t2(start_pos))
       )
    |> map(((start_pos, {item, span: (_, end_pos)})) =>
         start_end(start_pos, end_pos, Iv_variable(item))
       )
  );

let rec parse_value_literal = (is_const, parser) =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Bracket_open, _} =>
    parse_list_literal(is_const, parser)
  | {item: Graphql_lexer.Curly_open, _} =>
    parse_object_literal(is_const, parser)
  | {item: Graphql_lexer.Dollar, _} when !is_const =>
    parse_variable_literal(parser)
  | {item: Graphql_lexer.Int(i), _} => replace_next_token(parser, Iv_int(i))
  | {item: Graphql_lexer.Float(f), _} =>
    replace_next_token(parser, Iv_float(f))
  | {item: Graphql_lexer.String(s), _} =>
    replace_next_token(parser, Iv_string(s))
  | {item: Graphql_lexer.Name("true"), _} =>
    replace_next_token(parser, Iv_boolean(true))
  | {item: Graphql_lexer.Name("false"), _} =>
    replace_next_token(parser, Iv_boolean(false))
  | {item: Graphql_lexer.Name("null"), _} =>
    replace_next_token(parser, Iv_null)
  | {item: Graphql_lexer.Name(name), _} =>
    replace_next_token(parser, Iv_enum(name))
  | _ => map_next_token_with_error(parser, t => Unexpected_token(t))
  }
and parse_list_literal = (is_const, parser) =>
  delimited_list(
    parser,
    Graphql_lexer.Bracket_open,
    parse_value_literal(is_const),
    Graphql_lexer.Bracket_close,
  )
  |> Result_ext.map(span => map(items => Iv_list(items), span))
and parse_object_literal = (is_const, parser) =>
  delimited_list(
    parser,
    Graphql_lexer.Curly_open,
    parse_object_field(is_const),
    Graphql_lexer.Curly_close,
  )
  |> Result_ext.map(span => map(items => Iv_object(items), span))
and parse_object_field = (is_const, parser) =>
  Result_ext.(
    expect_name(parser)
    |> flat_map(key => expect(parser, Graphql_lexer.Colon) |> replace(key))
    |> flat_map(key =>
         parse_value_literal(is_const, parser) |> map(make_t2(key))
       )
  );
