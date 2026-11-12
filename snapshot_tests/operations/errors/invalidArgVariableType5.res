/**
 * ID should be equal to string, so the first line should not throw
 */
module MyQuery = %graphql(`
  query invalidArgValues($arg: String) {
    invalidArgValues(
        id: $arg
        stringRequired: $arg
    )
  }
`)
