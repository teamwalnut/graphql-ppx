[%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
  }

  fragment Another on Lists {
    nullableOfNullable
    ...ListFragment
  }
|}
];
