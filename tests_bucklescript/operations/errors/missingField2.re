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
    l1: thisTypeDoesNotExist {
      ...Fragments
    }
  }
|}
];
