/**
 * Should throw with type List
 */
module Case1 = %graphql(`
  query invalidDefaultValues {
    invalidDefaultValues(
        string: ["1", "2"]
    )
  }
`)
