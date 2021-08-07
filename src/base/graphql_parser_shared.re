open Graphql_ast;
open Graphql_parser;
open Source_pos;

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

let rec parse_type = parser =>
  Result_ext.(
    skip(parser, Graphql_lexer.Bracket_open)
    |> flat_map(
         fun
         | Some({span: (start_pos, _), _}) =>
           parse_type(parser)
           |> flat_map(inner_type =>
                expect(parser, Graphql_lexer.Bracket_close)
                |> map(make_t2(inner_type))
              )
           |> flat_map(
                ((inner_type, {span: (_, _ as bracket_end_pos), _})) =>
                switch (peek(parser)) {
                | {item: Graphql_lexer.Exclamation_mark, span: (_, end_pos)} =>
                  next(parser)
                  |> replace(
                       start_end(
                         start_pos,
                         end_pos,
                         Tr_non_null_list(inner_type),
                       ),
                     )

                | _ =>
                  Ok(
                    start_end(
                      start_pos,
                      bracket_end_pos,
                      Tr_list(inner_type),
                    ),
                  )
                }
              )

         | None =>
           expect_name(parser)
           |> flat_map(name =>
                switch (peek(parser)) {
                | {item: Graphql_lexer.Exclamation_mark, span: (_, end_pos)} =>
                  next(parser)
                  |> replace(
                       start_end(
                         start_pos(name),
                         end_pos,
                         Tr_non_null_named(name),
                       ),
                     )

                | _ => Ok(Source_pos.replace(name, Tr_named(name)))
                }
              ),
       )
  );
