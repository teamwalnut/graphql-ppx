open Test_shared

module My_query =
[%graphql
{|
  query ($query: Int!) {
    argNamedQuery(query: $query)
  }
|}]

let serializes_variables () =
  test_json_
    (My_query.makeVariables ~query:2 ()
    |> My_query.serializeVariables |> My_query.variablesToJson)
    (Json.Read.from_string {|{"query": 2}|})

let no_name_clash () = test_str_not My_query.query "a query"

let tests =
  [
    ("Serializes variables", serializes_variables);
    ("The name 'query' does not clash with the query argument", no_name_clash);
  ]
