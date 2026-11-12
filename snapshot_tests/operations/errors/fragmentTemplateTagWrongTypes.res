%graphql(`
  fragment ListFragment on Lists @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  fragment ListFragmentWrong on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  query MyQuery @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    lists {
      ...ListFragment
      ...ListFragmentWrong
    }
  }
`)
