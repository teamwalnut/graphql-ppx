open Result;
open Graphql_parser;
open Source_pos;
open Graphql_parser_directives;

let rec parse_type_ref = parser => {
  Schema.(
    switch (next(parser)) {
    | Ok({item: Graphql_lexer.Name(name), _}) =>
      switch (peek(parser)) {
      | {item: Graphql_lexer.Exclamation_mark, _} =>
        let _ = next(parser);
        Ok(NonNull(Named(name)));
      | _ => Ok(Named(name))
      }
    | Ok({item: Graphql_lexer.Bracket_open, _}) =>
      parse_type_ref(parser)
      |> Result_ext.flat_map(type_ref => {
           expect(parser, Graphql_lexer.Bracket_close)
           |> Result_ext.map(_ => type_ref)
         })
      |> Result_ext.flat_map(type_ref => {
           switch (peek(parser)) {
           | {item: Graphql_lexer.Exclamation_mark, span: _} =>
             let _ = next(parser);
             Ok(NonNull(List(type_ref)));
           | _ => Ok(List(type_ref))
           }
         })

    | Ok({item, span}) => Error({span, item: Unexpected_token(item)})
    | Error(e) => Error(e)
    }
  );
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
let get_description = parser => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.String(s), span: _} =>
    let _ = next(parser);
    Some(s);
  | _ => None
  };
};
let parse_type_argument = parser => {
  open Result_ext;
  let description = get_description(parser);

  expect_name(parser)
  |> flat_map(name => expect(parser, Graphql_lexer.Colon) |> replace(name))
  |> flat_map(name =>
       parse_type_ref(parser) |> map(type_ref => (name, type_ref))
     )
  |> flat_map(((name, type_ref)) => {
       parse_default_value(parser)
       |> map(default_value =>
            Schema.{
              am_name: name.item,
              am_arg_type: type_ref,
              am_default_value: default_value,
              am_description: description,
            }
          )
     });
};

let parse_type_arguments = parser =>
  switch (peek(parser)) {
  | {item: Graphql_lexer.Paren_open, _} =>
    delimited_nonempty_list(
      parser,
      Graphql_lexer.Paren_open,
      parse_type_argument,
      Graphql_lexer.Paren_close,
    )
    |> Result_ext.map(args => args.item)
  | _ => Ok([])
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
           let _ = next(parser);

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
           let _ = next(parser);
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

let parse_field = parser => {
  let description = get_description(parser);
  expect_name(parser)
  |> Result_ext.flat_map(({item: name}) => {
       parse_type_arguments(parser)
       |> Result_ext.flat_map(arguments => {
            expect(parser, Graphql_lexer.Colon)
            |> Result_ext.map(_ => arguments)
          })
       |> Result_ext.flat_map(arguments => {
            parse_type_ref(parser)
            |> Result_ext.flat_map(field_type => {
                 let deprecation_reason = parse_deprecated_directive(parser);

                 Ok(
                   Schema.{
                     fm_name: name,
                     fm_description: description,
                     fm_arguments: arguments,
                     fm_field_type: field_type,
                     fm_deprecation_reason: deprecation_reason,
                   },
                 );
               })
          })
     });
};

let parse_fields = parser => {
  let rec loop = acc => {
    switch (peek(parser)) {
    | {item: Graphql_lexer.Curly_close, _} =>
      let _ = next(parser);
      Ok(List.rev(acc));
    | _ =>
      let field = parse_field(parser);
      switch (field) {
      | Ok(field) => loop([field, ...acc])
      | Error(e) => Error(e)
      };
    };
  };
  loop([]);
};

let parse_input_field = parser => {
  parse_type_argument(parser);
};

let rec parse_input_fields = (parser, acc) => {
  switch (peek(parser)) {
  | {item: Graphql_lexer.Curly_close, _} =>
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
  let rec parse_implementations = (parser, acc) => {
    switch (expect_name(parser)) {
    | Ok({item: name}) =>
      let acc = [name, ...acc];
      switch (peek(parser)) {
      | {item: Graphql_lexer.Curly_open} => Ok(List.rev(acc))
      | {item: Graphql_lexer.Ampersand} => parse_implementations(parser, acc)
      | {item, span} =>
        print_endline("HWOWWOOWO");
        Error({span, item: Unexpected_token(item)});
      };
    | Error(e) => Error(e)
    };
  };

  switch (peek(parser)) {
  | {item: Graphql_lexer.Name("implements")} =>
    let _ = next(parser);
    parse_implementations(parser, []);
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
            |> Result_ext.flat_map(_ => {parse_fields(parser)})
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
       |> Result_ext.flat_map(_ => parse_fields(parser))
       |> Result_ext.map(fields => {
            Schema.{
              im_name: name,
              im_description: description,
              im_fields: fields,
            }
          })
     });
};

let rec add_to_schema = (parser, callback: string => Schema.schema_meta) => {
  let schema =
    expect(parser, Graphql_lexer.Colon)
    |> Result_ext.flat_map(_ => expect_name(parser))
    |> Result_ext.map(name => callback(name.item));

  switch (schema) {
  | Ok(schema) => parse_schema_fields(parser, schema)
  | Error(e) => Error(e)
  };
}
and parse_schema_fields = (parser, acc) => {
  switch (next(parser)) {
  | Ok({item: Graphql_lexer.Name("query")}) =>
    add_to_schema(parser, name => {...acc, sm_query_type: name})
  | Ok({item: Graphql_lexer.Name("mutation")}) =>
    add_to_schema(parser, name => {...acc, sm_mutation_type: Some(name)})
  | Ok({item: Graphql_lexer.Name("subscription")}) =>
    add_to_schema(parser, name => {...acc, sm_subscription_type: Some(name)})
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

[@tailcall]
let rec parse_union_types = (~acc=[], parser) => {
  let pipe = skip(parser, Graphql_lexer.Pipe);
  switch (acc, pipe) {
  | (_, Ok(Some(_)))
  | ([], Ok(None)) =>
    switch (expect_name(parser)) {
    | Ok(union) => parse_union_types(~acc=[union.item, ...acc], parser)
    | Error(e) => Error(e)
    }
  // (_, Error(_)) is EOF, which is valid (it's a peek)
  | (_, _) => Ok(List.rev(acc))
  };
};

let parse_union = (~description, parser) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.flat_map(name => {
       expect(parser, Graphql_lexer.Equals) |> Result_ext.map(_ => name.item)
     })
  |> Result_ext.flat_map(name =>
       parse_union_types(parser)
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

let get_directive_location = str => {
  Schema.(
    switch (str) {
    | "MUTATION" => Dl_mutation
    | "SUBSCRIPTION" => Dl_subscription
    | "FIELD" => Dl_field
    | "FRAGMENT_DEFINITION" => Dl_fragment_definition
    | "FRAGMENT_SPREAD" => Dl_fragment_spread
    | "INLINE_FRAGMENT" => Dl_inline_fragment
    | _ => Dl_unknown
    }
  );
};

let rec parse_directive_locations = (~acc=[], parser) => {
  let pipe = skip(parser, Graphql_lexer.Pipe);
  switch (acc, pipe) {
  //  first directive can be without leading pipe
  | ([], Ok(None))
  | (_, Ok(Some(_))) =>
    switch (expect_name(parser)) {
    | Ok({item: name}) =>
      parse_directive_locations(
        ~acc=[get_directive_location(name), ...acc],
        parser,
      )
    | Error(e) => Error(e)
    }
  // (_, Error(_)) is EOF, which is valid (it's a peek)
  | _ => Ok(List.rev(acc))
  };
};

let parse_directive =
    (~description as _, parser): result(Schema.directive_meta, _) => {
  expect_name(parser)
  |> Result_ext.flat_map(_ => expect(parser, Graphql_lexer.At))
  |> Result_ext.flat_map(_ => expect_name(parser))
  |> Result_ext.flat_map(({item: name}) => {
       parse_type_arguments(parser)
       |> Result_ext.map(arguments => (name, arguments))
     })
  |> Result_ext.flat_map(((name, arguments)) => {
       expect(parser, Graphql_lexer.Name("on"))
       |> Result_ext.flat_map(_ => {
            (
              parse_directive_locations(parser):
                result(list(Schema.directive_location), _)
            )
            |> Result_ext.map((locations) => {
                 (
                   Schema.{
                     dm_name: name,
                     //  dm_description: description,
                     dm_locations: locations,
                     dm_arguments: arguments,
                   }: Schema.directive_meta
                 )
               })
          })
     });
};

let parse_element: parser => result(schema_element, spanning(parseError)) =
  parser => {
    let description = get_description(parser);
    switch (peek(parser)) {
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
    | {item: Graphql_lexer.Name("directive"), _} =>
      parse_directive(~description, parser)
      |> Result_ext.map(directive => Directive(directive))
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

let add_built_in_scalars = type_map => {
  ["Int", "Float", "String", "ID", "Boolean"]
  |> List.iter(scalar =>
       Hashtbl.add(
         type_map,
         scalar,
         Schema.Scalar({sm_name: scalar, sm_description: None}),
       )
     );
  type_map;
};
let add_built_in_directives = directive_map => {
  open Schema;
  let directives = [
    {
      dm_name: "include",
      // dm_description: None,
      dm_locations: [Dl_field, Dl_fragment_spread, Dl_inline_fragment],
      dm_arguments: [
        {
          am_name: "if",
          am_description: None,
          am_arg_type: NonNull(Named("Boolean")),
          am_default_value: None,
        },
      ],
    },
    {
      dm_name: "skip",
      // dm_description: None,
      dm_locations: [Dl_field, Dl_fragment_spread, Dl_inline_fragment],
      dm_arguments: [
        {
          am_name: "if",
          am_description: None,
          am_arg_type: NonNull(Named("Boolean")),
          am_default_value: None,
        },
      ],
    },
  ];
  directives
  |> List.iter(directive =>
       Hashtbl.add(directive_map, directive.dm_name, directive)
     );
  directive_map;
};

let parse = parser => {
  let schema =
    Schema.{
      meta: {
        sm_query_type: "Query",
        sm_mutation_type: None,
        sm_subscription_type: None,
      },
      type_map: add_built_in_scalars(Hashtbl.create(100)),
      directive_map: add_built_in_directives(Hashtbl.create(10)),
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
