[%graphql
  {|
    fragment Dog on Dog {
      name
    }
    query  {
      hasuraRepro(orderBy: [{id: desc}]) {
        ...Dog
      }
    }
  |}
];
