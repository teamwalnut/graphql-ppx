%graphql(`
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

module WrongFragmentName = ListFragment

%graphql(`
  query MyQuery @ppxConfig(templateTag: "gql", templateTagReturnType: "bool") {
    lists {
      ...ListFragment
    }
  }
`)
