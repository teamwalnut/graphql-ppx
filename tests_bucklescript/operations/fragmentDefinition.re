module Fragments = [%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
|}
];

module MyQuery = [%graphql
  {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }
  }
|}
];
