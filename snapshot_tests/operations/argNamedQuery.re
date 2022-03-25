module MyQuery = [%graphql
  {|
  query ($query: Int!) {
    argNamedQuery(query: $query)
  }
|}
];

module MultipleQueryWithSameVarName = [%graphql
  {|
  query Query1($query: Int!) {
    argNamedQuery(query: $query)
  }
  query Query2($query: Int!) {
    argNamedQuery(query: $query)
  }
|}
];
