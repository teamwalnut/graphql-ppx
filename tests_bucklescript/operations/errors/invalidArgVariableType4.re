/**
 * Custom scalars should accept anything, so the first line should not throw
 * The second one should expect a generic Object type (no checking of fields), so it should throw
 */
module MyQuery = [%graphql
  {|
  query invalidArgValues($arg: EmbeddedInput!, $arg2: Int) {
    invalidArgValues(
        custom: $arg
        obj: $arg2
        stringRequired: ""
    )
  }
|}
];
