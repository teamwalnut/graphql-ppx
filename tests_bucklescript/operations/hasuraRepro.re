[%graphql
  {|
    fragment Dog on Dog {
      name
    }
    query HasuraRepro($blockNumber: Int) {
      hasuraRepro(orderBy: [{id: desc}], block: { number: $blockNumber }) {
        ...Dog
      }
    }
  |}
];
