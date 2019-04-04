open Result;
open Graphql_ast;

open Graphql_parser;
open Source_pos;

let last = l =>
  switch (List.length(l)) {
  | 0 => None
  | n => Some(List.nth(l, n - 1))
  };

let selection_end_pos = s =>
  switch (s) {
  | Field({span: (_, end_pos), _}) => end_pos
  | FragmentSpread({span: (_, end_pos), _}) => end_pos
  | InlineFragment({span: (_, end_pos), _}) => end_pos
  };

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

let parse_variable_definition = parser =>
  Result_ext.(
    expect(parser, Graphql_lexer.Dollar)
    |> flat_map(({span: (start_pos, _), _}) =>
         expect_name(parser) |> map(make_t2(start_pos))
       )
    |> flat_map(v => expect(parser, Graphql_lexer.Colon) |> replace(v))
    |> flat_map(((start_pos, name)) =>
         parse_type(parser) |> map(make_t3(start_pos, name))
       )
    |> flat_map(((start_pos, name, ty)) =>
         skip(parser, Graphql_lexer.Equals)
         |> map(make_t4(start_pos, name, ty))
       )
    |> flat_map(((start_pos, name, ty, equal_sign)) =>
         switch (equal_sign) {
         | None =>
           Ok(
             start_end(
               start_pos,
               end_pos(ty),
               (
                 start_end(start_pos, end_pos(name), name.item),
                 {vd_type: ty, vd_default_value: None},
               ),
             ),
           )
         | Some(_) =>
           Graphql_parser_value.parse_value_literal(true, parser)
           |> map(default_value =>
                start_end(
                  start_pos,
                  end_pos(default_value),
                  (
                    start_end(start_pos, end_pos(name), name.item),
                    {vd_type: ty, vd_default_value: Some(default_value)},
                  ),
                )
              )
         }
       )
  );

let parse_variable_definitions = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Paren_open, _} =>
    delimited_nonempty_list(
      parser,
      Graphql_lexer.Paren_open,
      parse_variable_definition,
      Graphql_lexer.Paren_close,
    )
    |> Result_ext.map(span =>
         Some(map(items => List.map(s => s.item, items), span))
       )
  | _ => Ok(None)
  };

let rec parse_selection_set = parser =>
  delimited_nonempty_list(
    parser,
    Graphql_lexer.Curly_open,
    parse_selection,
    Graphql_lexer.Curly_close,
  )
and parse_optional_selection_set = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Curly_open, _} =>
    Result_ext.map(x => Some(x), parse_selection_set(parser))
  | _ => Ok(None)
  }
and parse_field = parser => {
  open Result_ext;
  let parse_rest = (alias, name) =>
    parse_arguments(parser)
    |> flat_map(arguments =>
         parse_directives(parser) |> map(make_t2(arguments))
       )
    |> flat_map(((arguments, directives)) =>
         parse_optional_selection_set(parser)
         |> map(make_t3(arguments, directives))
       )
    |> map(((arguments, directives, selection_set)) =>
         start_end(
           switch (alias) {
           | Some({span: (start_pos, _), _}) => start_pos
           | None => name |> start_pos
           },
           switch (selection_set) {
           | Some({span: (_, end_pos), _}) => end_pos
           | None =>
             switch (last(directives)) {
             | Some({span: (_, end_pos), _}) => end_pos
             | None =>
               switch (arguments) {
               | Some({span: (_, end_pos), _}) => end_pos
               | None => name |> end_pos
               }
             }
           },
           {
             fd_alias: alias,
             fd_name: name,
             fd_arguments: arguments,
             fd_directives: directives,
             fd_selection_set: selection_set,
           },
         )
       );

  expect_name(parser)
  |> flat_map(alias_or_name =>
       skip(parser, Graphql_lexer.Colon) |> map(make_t2(alias_or_name))
     )
  |> flat_map(((alias_or_name, colon)) =>
       switch (colon) {
       | None => parse_rest(None, alias_or_name)
       | Some(_) =>
         expect_name(parser)
         |> flat_map(name => parse_rest(Some(alias_or_name), name))
       }
     );
}
and parse_fragment = parser =>
  Result_ext.(
    switch (expect(parser, Graphql_lexer.Ellipsis)) {
    | Error(e) => Error(e)
    | Ok({span: (start_pos, _), _}) =>
      switch (peek(parser)) {
      | {item: Graphql_lexer.Name("on"), _} =>
        next(parser)
        |> flat_map(_ => expect_name(parser))
        |> flat_map(name => parse_directives(parser) |> map(make_t2(name)))
        |> flat_map(((name, directives)) =>
             parse_selection_set(parser) |> map(make_t3(name, directives))
           )
        |> map(((name, directives, selection_set)) =>
             InlineFragment(
               start_end(
                 start_pos,
                 end_pos(selection_set),
                 {
                   if_type_condition: Some(name),
                   if_directives: directives,
                   if_selection_set: selection_set,
                 },
               ),
             )
           )

      | {item: Graphql_lexer.Curly_open, _} =>
        parse_selection_set(parser)
        |> map(selection_set =>
             InlineFragment(
               start_end(
                 start_pos,
                 end_pos(selection_set),
                 {
                   if_type_condition: None,
                   if_directives: [],
                   if_selection_set: selection_set,
                 },
               ),
             )
           )

      | {item: Graphql_lexer.Name(_), _} =>
        expect_dotted_name(parser)
        |> flat_map(name => parse_directives(parser) |> map(make_t2(name)))
        |> map(((name, directives)) =>
             FragmentSpread(
               start_end(
                 start_pos,
                 switch (last(directives)) {
                 | Some(s) => end_pos(s)
                 | None => end_pos(name)
                 },
                 {fs_name: name, fs_directives: directives},
               ),
             )
           )

      | {item: Graphql_lexer.At, _} =>
        parse_directives(parser)
        |> flat_map(directives =>
             parse_selection_set(parser) |> map(make_t2(directives))
           )
        |> map(((directives, selection_set)) =>
             InlineFragment(
               start_end(
                 start_pos,
                 end_pos(selection_set),
                 {
                   if_type_condition: None,
                   if_directives: directives,
                   if_selection_set: selection_set,
                 },
               ),
             )
           )

      | _ =>
        switch (next(parser)) {
        | Error(e) => Error(e)
        | Ok(span) => Error(Source_pos.map(t => Unexpected_token(t), span))
        }
      }
    }
  )
and parse_selection = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Ellipsis, _} => parse_fragment(parser)
  | _ =>
    Result_ext.map(
      (span: spanning(field)) => Field(span),
      parse_field(parser),
    )
  };

let parse_operation_type = parser =>
  switch (next(parser)) {
  | Error(e) => Error(e)
  | Ok({item: Graphql_lexer.Name("query"), _} as span) =>
    Ok(replace(span, Query))
  | Ok({item: Graphql_lexer.Name("mutation"), _} as span) =>
    Ok(replace(span, Mutation))
  | Ok({item: Graphql_lexer.Name("subscription"), _} as span) =>
    Ok(replace(span, Subscription))
  | Ok(span) => Error(map(t => Unexpected_token(t), span))
  };

let parse_operation_definition = parser =>
  Result_ext.(
    switch (peek(parser)) {
    | {item: Graphql_lexer.Curly_open, _} =>
      switch (parse_selection_set(parser)) {
      | Error(e) => Error(e)
      | Ok(span) =>
        Ok(
          Source_pos.replace(
            span,
            {
              o_type: Query,
              o_name: None,
              o_variable_definitions: None,
              o_directives: [],
              o_selection_set: span,
            },
          ),
        )
      }
    | {span: (start_pos, _), _} =>
      let parse_rest = (operation_type, name) =>
        parse_variable_definitions(parser)
        |> flat_map(vdefs =>
             parse_directives(parser) |> map(make_t2(vdefs))
           )
        |> flat_map(((vdefs, directives)) =>
             parse_selection_set(parser) |> map(make_t3(vdefs, directives))
           )
        |> map(((vdefs, directives, selection_set)) =>
             start_end(
               start_pos,
               end_pos(selection_set),
               {
                 o_type: operation_type,
                 o_name: name,
                 o_variable_definitions: vdefs,
                 o_directives: directives,
                 o_selection_set: selection_set,
               },
             )
           );

      switch (parse_operation_type(parser)) {
      | Error(e) => Error(e)
      | Ok(operation_type) =>
        switch (peek(parser)) {
        | {item: Graphql_lexer.Name(_), _} =>
          switch (expect_name(parser)) {
          | Error(e) => Error(e)
          | Ok(name_span) =>
            parse_rest(operation_type.item, Some(name_span))
          }
        | _ => parse_rest(operation_type.item, None)
        }
      };
    }
  );

let parse_fragment_definition = parser =>
  Result_ext.(
    expect(parser, Graphql_lexer.Name("fragment"))
    |> flat_map(({span: (start_pos, _), _}) =>
         expect_name(parser) |> map(make_t2(start_pos))
       )
    |> flat_map(((start_pos, name)) =>
         switch (name) {
         | {item: "on", _} as span =>
           Error(
             Source_pos.replace(
               span,
               Unexpected_token(Graphql_lexer.Name("on")),
             ),
           )
         | _ => [@implicit_arity] Ok(start_pos, name)
         }
       )
    |> flat_map(v =>
         expect(parser, Graphql_lexer.Name("on")) |> replace(v)
       )
    |> flat_map(((start_pos, name)) =>
         expect_name(parser) |> map(make_t3(start_pos, name))
       )
    |> flat_map(((start_pos, name, type_cond)) =>
         parse_directives(parser)
         |> map(make_t4(start_pos, name, type_cond))
       )
    |> flat_map(((start_pos, name, type_cond, directives)) =>
         parse_selection_set(parser)
         |> map(make_t5(start_pos, name, type_cond, directives))
       )
    |> map(((start_pos, name, type_cond, directives, selection_set)) =>
         start_end(
           start_pos,
           end_pos(selection_set),
           {
             fg_name: name,
             fg_type_condition: type_cond,
             fg_directives: directives,
             fg_selection_set: selection_set,
           },
         )
       )
  );

let parse_definition = parser =>
  Result_ext.(
    switch (peek(parser)) {
    | {item: Graphql_lexer.Curly_open, _}
    | {item: Graphql_lexer.Name("query"), _}
    | {item: Graphql_lexer.Name("mutation"), _}
    | {item: Graphql_lexer.Name("subscription"), _} =>
      parse_operation_definition(parser) |> map(def => Operation(def))

    | {item: Graphql_lexer.Name("fragment"), _} =>
      parse_fragment_definition(parser) |> map(def => Fragment(def))

    | span => Error(Source_pos.map(t => Unexpected_token(t), span))
    }
  );

let parse_document = parser => {
  let rec scanner = acc =>
    switch (parse_definition(parser)) {
    | Error(e) => Error(e)
    | Ok(def) =>
      switch (peek(parser)) {
      | {item: Graphql_lexer.End_of_file, _} => Ok(List.rev([def, ...acc]))
      | _ => scanner([def, ...acc])
      }
    };

  scanner([]);
};
