/**
 * Required argument error should take prioroty over type error
 * Expecting this to be a required related error, not a type one
 */
module MyQuery = %graphql(`
  query invalidArgValues {
    invalidArgValues(
        stringRequired: $arg
    )
  }
`)
