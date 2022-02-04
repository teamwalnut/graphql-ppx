module MyQuery = [%graphql
  {|
  query {
    deprecatedFields {
      field1
      field2
      deprecatedField
    }
  }
|}
];
