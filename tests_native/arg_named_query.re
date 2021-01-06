open Test_shared;

module My_query = [%graphql
  {|
  query ($query: Int!) {
    argNamedQuery(query: $query)
  }
|}
];

let serializes_variables = () =>
  test_json(
    My_query.makeVariables(~query=2, ()),
    Yojson.Basic.from_string({|{"query": 2}|}),
  );

let no_name_clash = () =>
  Alcotest.(check(neg(string), "strings", My_query.query, "a query"));

let tests = [
  ("Serializes variables", `Quick, serializes_variables),
  (
    "The name 'query' does not clash with the query argument",
    `Quick,
    no_name_clash,
  ),
];
