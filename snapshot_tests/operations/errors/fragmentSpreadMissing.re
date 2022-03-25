[%graphql
  {|
  query MyQuery {
    lists {
      ...MissingFragment
    }
  }
  |}
];
