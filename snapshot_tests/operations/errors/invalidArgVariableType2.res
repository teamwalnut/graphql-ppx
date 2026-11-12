/**
 * If an arg is required but a variable is not, it should throw an error
 */
module MyQuery = %graphql(`
  query invalidArgValues($arg: String) {
    invalidArgValues(
        stringRequired: $arg
    )
  }
`)
