open Graphql_ast;
open Graphql_parser;
open Source_pos;

// This is shared between document and schema parsing.
// This should be rewritten for schema, but now it's only really
// used for the @deprecated directive on types.
// In the latest spec more directives are possible, so it needs separate
// AST as part of Schema.t

let parse_argument = parser =>
  Result_ext.(
    expect_name(parser)
    |> flat_map(name =>
         expect(parser, Graphql_lexer.Colon) |> replace(name)
       )
    |> flat_map(name =>
         Graphql_parser_value.parse_value_literal(false, parser)
         |> map(value => (name, value))
       )
  );

let parse_arguments = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Paren_open, _} =>
    delimited_nonempty_list(
      parser,
      Graphql_lexer.Paren_open,
      parse_argument,
      Graphql_lexer.Paren_close,
    )
    |> Result_ext.map(args => Some(args))
  | _ => Ok(None)
  };

let parse_directive = parser =>
  Result_ext.(
    expect(parser, Graphql_lexer.At)
    |> flat_map(({span: (at_start, _), _}) =>
         expect_name(parser) |> map(make_t2(at_start))
       )
    |> flat_map(((at_start, name)) =>
         parse_arguments(parser) |> map(make_t3(at_start, name))
       )
    |> map(((at_start, name, arguments)) =>
         switch (arguments) {
         | None =>
           start_end(
             at_start,
             end_pos(name),
             {d_name: name, d_arguments: None},
           )
         | Some(arguments) =>
           start_end(
             at_start,
             switch (arguments.item) {
             | [hd, ..._] => hd |> snd |> end_pos
             | [] => name |> end_pos
             },
             {d_name: name, d_arguments: Some(arguments)},
           )
         }
       )
  );

let parse_directives = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.At, _} =>
    let rec scanner = acc =>
      switch (peek(parser)) {
      | {item: Graphql_lexer.At, _} =>
        switch (parse_directive(parser)) {
        | Error(e) => Error(e)
        | Ok(directive) => scanner([directive, ...acc])
        }
      | _ => Ok(List.rev(acc))
      };

    scanner([]);
  | _ => Ok([])
  };
