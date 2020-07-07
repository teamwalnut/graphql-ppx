[%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    ...MissingFragment
  }
|}
];
