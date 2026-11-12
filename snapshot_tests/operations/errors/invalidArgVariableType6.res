/**
 * Lists should be generic and not their innermost type, so this should fail
 */
module MyQuery = %graphql(`
  query invalidArgValues($arg: String) {
    invalidArgValues(
        list: $arg
        stringRequired: ""
    )
  }
`)
