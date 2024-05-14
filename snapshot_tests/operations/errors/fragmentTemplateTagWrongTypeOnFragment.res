%graphql(`
  fragment ListFragmentInner on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  fragment ListFragment on Lists  @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    nullableOfNullable
    ...ListFragmentInner
  }
`)
