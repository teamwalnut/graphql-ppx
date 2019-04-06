open Result;
open Source_pos;

type parser = {mutable tokens: list(spanning(Graphql_lexer.token))};

type parseError =
  | Unexpected_token(Graphql_lexer.token)
  | Unexpected_end_of_file
  | Lexer_error(Graphql_lexer.lexerError);

let make = tokens => {tokens: tokens};

let peek = parser => List.hd(parser.tokens);

exception Internal_parse_error;

let next = parser =>
  switch (parser.tokens) {
  | [x] => Error(replace(x, Unexpected_end_of_file))
  | [x, ..._] =>
    let () = parser.tokens = List.tl(parser.tokens);
    Ok(x);
  | _ => raise(Internal_parse_error)
  };

let expect = (parser, token) =>
  switch (next(parser)) {
  | Ok(span) when span.item == token => Ok(span)
  | Ok(span) => Error(replace(span, Unexpected_token(span.item)))
  | x => x
  };

let expect_name = parser =>
  switch (next(parser)) {
  | Ok({item: Graphql_lexer.Name(name), _} as span) =>
    Ok(replace(span, name))
  | Ok({item: Graphql_lexer.End_of_file, _} as span) =>
    Error(replace(span, Unexpected_end_of_file))
  | Ok(span) => Error(map(t => Unexpected_token(t), span))
  | Error(e) => Error(e)
  };

let expect_dotted_name = parser => {
  let rec loop = (start_pos, _, acc) =>
    switch (next(parser)) {
    | Ok({item: Graphql_lexer.Name(name), span: (_, end_pos)}) =>
      let acc = acc ++ name;
      switch (peek(parser)) {
      | {item: Graphql_lexer.Dot, span: (_, end_pos)} =>
        let _ = next(parser);
        loop(start_pos, end_pos, acc ++ ".");
      | _ => Ok(start_end(start_pos, end_pos, acc))
      };
    | Ok({item: Graphql_lexer.End_of_file, _} as span) =>
      Error(replace(span, Unexpected_end_of_file))
    | Ok(span) => Error(map(t => Unexpected_token(t), span))
    | Error(e) => Error(e)
    };

  let {span: (start_pos, end_pos), _} = peek(parser);
  loop(start_pos, end_pos, "");
};

let skip = (parser, token) =>
  switch (peek(parser)) {
  | span when span.item == token =>
    Result_ext.map(x => Some(x), next(parser))
  | span when span.item == Graphql_lexer.End_of_file =>
    Error(zero_width(start_pos(span), Unexpected_end_of_file))
  | _ => Ok(None)
  };

let delimited_list = (parser, opening, sub_parser, closing) =>
  switch (expect(parser, opening)) {
  | Error(e) => Error(e)
  | Ok({span: (start_pos, _), _}) =>
    let rec scanner = acc =>
      switch (skip(parser, closing)) {
      | Ok(Some({span: (_, end_pos), _})) =>
        Ok(start_end(start_pos, end_pos, List.rev(acc)))
      | _ =>
        switch (sub_parser(parser)) {
        | Ok(span) => scanner([span, ...acc])
        | Error(e) => Error(e)
        }
      };

    scanner([]);
  };

let delimited_nonempty_list = (parser, opening, sub_parser, closing) =>
  switch (expect(parser, opening)) {
  | Error(e) => Error(e)
  | Ok({span: (start_pos, _), _}) =>
    let rec scanner = acc =>
      switch (sub_parser(parser)) {
      | Error(e) => Error(e)
      | Ok(span) =>
        switch (skip(parser, closing)) {
        | Error(e) => Error(e)
        | Ok(Some({span: (_, end_pos), _})) =>
          Ok(start_end(start_pos, end_pos, List.rev([span, ...acc])))
        | Ok(None) => scanner([span, ...acc])
        }
      };

    scanner([]);
  };
