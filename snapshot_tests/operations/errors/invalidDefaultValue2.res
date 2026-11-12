/**
 * Should throw with type \"InputObject\"
 */
module Case1 = %graphql(`
  query invalidDefaultValues {
    invalidDefaultValues(
        int: {test: "xxx"}
    )
  }
`)
