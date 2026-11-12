%graphql(`
  fragment ListFragment on Lists @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  query MyQuery {
    lists {
      ...ListFragment
    }
  }
`)
