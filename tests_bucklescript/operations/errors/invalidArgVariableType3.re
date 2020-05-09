/**
 * These are just mismatched types, the optional string should throw an error
 */
module MyQuery = [%graphql
  {|
  query invalidArgValues($arg: String!, $arg2: Int) {
    invalidArgValues(
        stringRequired: $arg
        string: $arg2
    )
  }
|}
];
