/**
 * Should not throw
 */
module Case1 = %graphql(`
  query invalidDefaultValues {
    invalidDefaultValues(
        id: "Some Id"
    )
  }
`)

@ocaml.doc("
 * Should throw with type \"Int\"
 ")
module Case2 = %graphql(`
  query invalidDefaultValues {
    invalidDefaultValues(
        id: 5
    )
  }
`)
