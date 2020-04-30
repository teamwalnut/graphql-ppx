module Fragment = [%graphql
  {|
  fragment ListFragment on UnknownFragment {
    nullableOfNullable
    nullableOfNonNullable
  }
|}
];
