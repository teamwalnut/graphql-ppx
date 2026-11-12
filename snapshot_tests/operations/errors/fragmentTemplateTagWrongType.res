%graphql(`
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  query MyQuery @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    lists {
      ...ListFragment
    }
  }
`)
