open Result;
open Graphql_parser;
open Source_pos;
open Graphql_parser_shared;

let rec parse_type_ref = parser => {
  switch (next(parser)) {
  | Ok({item: Graphql_lexer.Name(name), span}) =>
    switch (peek(parser)) {
    | {item: Graphql_lexer.Exclamation_mark, span: excl_span} =>
      let _ = next(parser);
      Ok(
        Graphql_ast.Tr_non_null_named(
          Source_pos.start_end(fst(span), snd(excl_span), name),
        ),
      );
    | _ => Ok(Graphql_ast.Tr_named({item: name, span}))
    }
  | Ok({item: Graphql_lexer.Bracket_open, span}) =>
    parse_type_ref(parser)
    |> Result_ext.flat_map(type_ref => {
         expect(parser, Graphql_lexer.Bracket_close)
         |> Result_ext.map(_ => type_ref)
       })
    |> Result_ext.flat_map(type_ref => {
         switch (peek(parser)) {
         | {item: Graphql_lexer.Exclamation_mark, span: excl_span} =>
           let _ = next(parser);

           Ok(
             Graphql_ast.Tr_non_null_list(
               Source_pos.start_end(fst(span), snd(excl_span), type_ref),
             ),
           );
         | _ => Ok(Graphql_ast.Tr_list({span, item: type_ref}))
         }
       })

  | Ok({item, span}) => Error({span, item: Unexpected_token(item)})
  | Error(e) => Error(e)
  };
};

let parse_default_value = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Equals} =>
    let _ = next(parser);
    Graphql_parser_value.parse_value_literal(true, parser)
    |> Result_ext.map(default_value =>
         Some(Graphql_printer.print_input_value(default_value.item))
       );

  | _ => Ok(None)
  };
};
let parse_type_argument = parser =>
  Result_ext.(
    expect_name(parser)
    |> flat_map(name =>
         expect(parser, Graphql_lexer.Colon) |> replace(name)
       )
    |> flat_map(name =>
         parse_type_ref(parser) |> map(value => (name, value))
       )
    |> flat_map(((name, value)) => {
         parse_default_value(parser)
         |> map(default_value => (name, value, default_value))
       })
  );

let parse_type_arguments = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Paren_open, _} =>
    delimited_nonempty_list(
      parser,
      Graphql_lexer.Paren_open,
      parse_type_argument,
      Graphql_lexer.Paren_close,
    )
    |> Result_ext.map(args => Some(args))
  | _ => Ok(None)
  };

let get_description_and_peek = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.String(s), span: _} =>
    let _ = next(_);
    (Some(s), peek(parser));
  | other => (None, other)
  };
};

let get_description = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.String(s), span: _} =>
    let _ = next(_);
    Some(s);
  | _ => None
  };
};
let get_description_and_expect_name = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.String(description), span: _} =>
    let _ = next(parser);
    expect_name(parser) |> Result_ext.map(name => (Some(description), name));
  | _ => expect_name(parser) |> Result_ext.map(name => (None, name))
  };
};

type schema_element =
  | Type(Schema.type_meta)
  | Directive(Schema.directive_meta)
  | Schema(Schema.schema_meta);

let parse_scalar = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.map(({item: sm_name}) => {
       Schema.{sm_name, sm_description: description}
     });
};

let get_deprecated_directive = directives => {
  switch (directives) {
  | Ok([
      {
        item:
          Graphql_ast.{
            d_name: {item: "deprecated"},
            d_arguments:
              Some({
                item: [({item: "reason"}, {item: Iv_string(reason)})],
              }),
          },
      },
    ]) =>
    Some(reason)
  | _ => None
  };
};

let parse_deprecated_directive = parser => {
  parse_directives(parser) |> get_deprecated_directive;
};

let parse_enum_value = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.map(n => {
       // only accepted directive here is @deprecated(reason: "...")
       let evm_deprecation_reason = parse_deprecated_directive(parser);

       Schema.{
         evm_name: n.item,
         evm_description: description,
         evm_deprecation_reason,
       };
     });
};

let parse_enum = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => {expect_name(parser)})
  |> Result_ext.flat_map(({item: em_name}) => {
       expect(parser, Graphql_lexer.Curly_open)
       |> Result_ext.map(_ => em_name)
     })
  |> Result_ext.flat_map(em_name => {
       [@tailcall]
       let rec consume_enums = acc => {
         switch (peek(parser)) {
         // enum with description
         | {item: Graphql_lexer.String(description)} =>
           let _ = next(_);

           switch (parse_enum_value(~description=Some(description), parser)) {
           | Ok(value) => consume_enums([value, ...acc])
           | Error(e) => Error(e)
           };
         // just enum
         | {item: Graphql_lexer.Name(_)} =>
           switch (parse_enum_value(~description=None, parser)) {
           | Ok(value) => consume_enums([value, ...acc])
           | Error(e) => Error(e)
           }
         | {item: Graphql_lexer.Curly_close} =>
           let _ = next(_);
           // rev the list, because we been adding to the head
           Ok(List.rev(acc));
         | {item, span} => Error({span, item: Unexpected_token(item)})
         };
       };

       let em_values = consume_enums([]);

       em_values
       |> Result_ext.map(em_values => {
            Schema.{em_name, em_description: description, em_values}
          });
     });
};

let rec parse_implements = (parser, acc) => {
  switch (expect_name(parser)) {
  | Ok({item: name}) =>
    let acc = [name, ...acc];
    switch (peek(parser)) {
    | {item: Graphql_lexer.Bracket_open} => Ok(List.rev(acc))
    | {item: Graphql_lexer.Ampersand} => parse_implements(parser, acc)
    | {item, span} => Error({span, item: Unexpected_token(item)})
    };
  | Error(e) => Error(e)
  };
};

let rec type_ref_to_schema = ast_type_ref => {
  switch (ast_type_ref) {
  | Graphql_ast.Tr_named({item: name}) => Schema.Named(name)
  | Tr_list({item: type_ref}) => type_ref_to_schema(type_ref)
  | Tr_non_null_named({item: name}) => NonNull(Named(name))
  | Tr_non_null_list({item: type_ref}) =>
    NonNull(type_ref_to_schema(type_ref))
  };
};

let parse_field = parser => {
  get_description_and_expect_name(parser)
  |> Result_ext.flat_map(((description, {item: name})) => {
       parse_type_arguments(parser)
       |> Result_ext.flat_map(arguments => {
            expect(parser, Graphql_lexer.Colon)
            |> Result_ext.map(_ => {
                 switch (arguments) {
                 | Some({item: arguments, _}) =>
                   arguments
                   |> List.map(
                        (({item: am_name, _}, type_ref, default_value)) => {
                        let am_arg_type = type_ref_to_schema(type_ref);
                        Schema.{
                          am_name,
                          am_description: None,
                          am_arg_type,
                          am_default_value: default_value,
                        };
                      })
                 | None => []
                 }
               })
          })
       |> Result_ext.flat_map(arguments => {
            parse_type(parser)
            |> Result_ext.flat_map(
                 (field_type: spanning(Graphql_ast.type_ref)) => {
                 let deprecation_reason = parse_deprecated_directive(parser);

                 Ok(
                   Schema.{
                     fm_name: name,
                     fm_description: description,
                     fm_arguments: arguments,
                     fm_field_type: type_ref_to_schema(field_type.item),
                     fm_deprecation_reason: deprecation_reason,
                   },
                 );
               })
          })
     });
};

let rec parse_fields = (parser, acc) => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Bracket_close, _} =>
    let _ = next(parser);
    Ok(List.rev(acc));
  | _ =>
    let field = parse_field(parser);
    switch (field) {
    | Ok(field) => parse_fields(parser, [field, ...acc])
    | Error(e) => Error(e)
    };
  };
};

let parse_input_field = parser => {
  let description = get_description(parser);
  parse_type_argument(parser)
  |> Result_ext.map((({item: name}, type_ref, default_value)) => {
       Schema.{
         am_name: name,
         am_description: description,
         am_arg_type: type_ref_to_schema(type_ref),
         am_default_value: default_value,
       }
     });
};

let rec parse_input_fields = (parser, acc) => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Bracket_close, _} =>
    let _ = next(parser);
    Ok(List.rev(acc));
  | _ =>
    let field = parse_input_field(parser);
    switch (field) {
    | Ok(field) => parse_input_fields(parser, [field, ...acc])
    | Error(e) => Error(e)
    };
  };
};

let parse_implements = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Name("implements")} =>
    let _ = next(parser);
    parse_implements(parser, []);
  | _ => Ok([])
  };
};

let parse_object = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => {expect_name(parser)})
  |> Result_ext.flat_map(({item: name, _}) => {
       parse_implements(parser)
       |> Result_ext.flat_map(implements => {
            expect(parser, Graphql_lexer.Curly_open)
            |> Result_ext.flat_map(_ => {parse_fields(parser, [])})
            |> Result_ext.map(fields => {
                 Schema.{
                   om_name: name,
                   om_description: description,
                   om_fields: fields,
                   om_interfaces: implements,
                 }
               })
          })
     });
};

let parse_interface = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.flat_map(({item: name, _}) => {
       expect(parser, Graphql_lexer.Curly_open)
       |> Result_ext.flat_map(_ => parse_fields(parser, []))
       |> Result_ext.map(fields => {
            Schema.{
              im_name: name,
              im_description: description,
              im_fields: fields,
            }
          })
     });
};

let rec parse_schema_fields = (parser, acc) => {
  switch (next(parser)) {
  | Ok({item: Graphql_lexer.Name("query")}) =>
    let schema =
      expect(parser, Graphql_lexer.Colon)
      |> Result_ext.flat_map(_ => expect_name(parser))
      |> Result_ext.map(name => Schema.{...acc, sm_query_type: name.item});

    switch (schema) {
    | Ok(schema) => parse_schema_fields(parser, schema)
    | Error(e) => Error(e)
    };

  | Ok({item: Graphql_lexer.Name("mutation")}) =>
    let schema =
      expect(parser, Graphql_lexer.Colon)
      |> Result_ext.flat_map(_ => expect_name(parser))
      |> Result_ext.map(name =>
           Schema.{...acc, sm_mutation_type: Some(name.item)}
         );

    switch (schema) {
    | Ok(schema) => parse_schema_fields(parser, schema)
    | Error(e) => Error(e)
    };
  | Ok({item: Graphql_lexer.Name("subscription")}) =>
    let schema =
      expect(parser, Graphql_lexer.Colon)
      |> Result_ext.flat_map(_ => expect_name(parser))
      |> Result_ext.map(name =>
           Schema.{...acc, sm_subscription_type: Some(name.item)}
         );

    switch (schema) {
    | Ok(schema) => parse_schema_fields(parser, schema)
    | Error(e) => Error(e)
    };
  | Ok({item: Graphql_lexer.Curly_close}) => Ok(acc)
  | Ok({item, span}) => Error({span, item: Unexpected_token(item)})
  | Error(e) => Error(e)
  };
};

let parse_schema = parser => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => {expect(parser, Graphql_lexer.Curly_open)})
  |> Result_ext.flat_map(_ =>
       parse_schema_fields(
         parser,
         {
           sm_query_type: "Query",
           sm_mutation_type: None,
           sm_subscription_type: None,
         },
       )
     );
};

let rec parse_union_types = (parser, acc) => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Pipe, _} =>
    let _ = next(parser);
    switch (expect_name(parser)) {
    | Ok(union) => parse_union_types(parser, [union.item, ...acc])
    | Error(e) => Error(e)
    };
  | _ => Ok(List.rev(acc))
  };
};

let parse_union = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.flat_map(name => {
       expect(parser, Graphql_lexer.Equals) |> Result_ext.map(_ => name.item)
     })
  |> Result_ext.flat_map(name =>
       expect_name(parser)
       |> Result_ext.flat_map(first_union => {
            parse_union_types(parser, [first_union.item])
          })
       |> Result_ext.map(unions => {
            Schema.{
              um_name: name,
              um_description: description,
              um_of_types: unions,
            }
          })
     );
};

let parse_input_object = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.flat_map(({item: name, _}) => {
       expect(parser, Graphql_lexer.Curly_open)
       |> Result_ext.flat_map(_ => parse_input_fields(parser, []))
       |> Result_ext.map(fields => {
            Schema.{
              iom_name: name,
              iom_description: description,
              iom_input_fields: fields,
            }
          })
     });
};

let parse_element: parser => result(schema_element, spanning(parseError)) =
  parser => {
    let (description, token) = get_description_and_peek(parser);
    switch (token) {
    | {item: Graphql_lexer.Name("scalar"), _} =>
      parse_scalar(~description, parser)
      |> Result_ext.map(scalar => Type(Scalar(scalar)))
    | {item: Graphql_lexer.Name("enum"), _} =>
      parse_enum(~description, parser)
      |> Result_ext.map(enum => Type(Enum(enum)))
    | {item: Graphql_lexer.Name("type"), _} =>
      parse_object(~description, parser)
      |> Result_ext.map(obj => Type(Object(obj)))
    | {item: Graphql_lexer.Name("interface"), _} =>
      parse_interface(~description, parser)
      |> Result_ext.map(interface => Type(Interface(interface)))
    | {item: Graphql_lexer.Name("union"), _} =>
      parse_union(~description, parser)
      |> Result_ext.map(union => Type(Union(union)))
    | {item: Graphql_lexer.Name("input"), _} =>
      parse_input_object(~description, parser)
      |> Result_ext.map(input_object => Type(InputObject(input_object)))
    | {item: Graphql_lexer.Name("schema"), _} =>
      parse_schema(parser) |> Result_ext.map(schema => Schema(schema))
    | {item, span} => Error({span, item: Unexpected_token(item)})
    };
  };

let add_to_schema = (schema: Schema.t, element) => {
  switch (element) {
  | Type(type_) =>
    Hashtbl.add(schema.type_map, Schema.type_name(type_), type_);
    schema;
  | Schema(meta) => Schema.{...schema, meta}
  | Directive(_) => schema
  };
};
let parse_elements = parser => {
  let schema =
    Schema.{
      meta: {
        sm_query_type: "Query",
        sm_mutation_type: None,
        sm_subscription_type: None,
      },
      type_map: Hashtbl.create(100),
      directive_map: Hashtbl.create(10),
    };
  let rec scanner = schema =>
    switch (parse_element(parser)) {
    | Error(e) => Error(e)
    | Ok(element) =>
      switch (peek(parser)) {
      | {item: Graphql_lexer.End_of_file, _} =>
        Ok(add_to_schema(schema, element))
      | _ => scanner(add_to_schema(schema, element))
      }
    };
  scanner(schema);
};
