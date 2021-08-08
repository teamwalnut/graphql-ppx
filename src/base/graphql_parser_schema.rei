let parse:
  Graphql_parser.parser =>
  result(Schema.t, Source_pos.spanning(Graphql_parser.parseError));
