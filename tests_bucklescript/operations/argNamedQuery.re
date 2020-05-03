module MyQuery = [%graphql
  {|
  query ($query: Int!) {
    argNamedQuery(query: $query)
  }
|}
];
