module InlineFragmentQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}
];
