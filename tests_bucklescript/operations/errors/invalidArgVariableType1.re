/**
 * Mismatched types shold take priority over required status
 * Expecting this to be a type related error, not a required one
 */
module MyQuery = [%graphql
  {|
  query invalidArgValues($arg: Int) {
    invalidArgValues(
        stringRequired: $arg
    )
  }
|}
];
