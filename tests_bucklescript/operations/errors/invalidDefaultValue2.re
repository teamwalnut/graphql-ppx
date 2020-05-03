/**
 * Should throw with type "Object"
 */
module Case1 = [%graphql
  {|
  query invalidDefaultValues {
    invalidDefaultValues(
        int: {test: "xxx"}
    )
  }
|}
];
