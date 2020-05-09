/**
 * Should not throw
 */
module Case1 = [%graphql
  {|
  query invalidDefaultValues {
    invalidDefaultValues(
        custom: 5
    )
  }
|}
];

/**
 * Should fail with "String"
 */
module Case2 = [%graphql
  {|
  query invalidDefaultValues {
    invalidDefaultValues(
        int: "Some string"
    )
  }
|}
];
