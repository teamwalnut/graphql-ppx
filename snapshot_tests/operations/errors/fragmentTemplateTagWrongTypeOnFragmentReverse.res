%graphql(`
  fragment ListFragmentInner on Lists @ppxConfig(templateTagLocation: "gql", templateTagReturnType: "bool") {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

%graphql(`
  fragment ListFragment on Lists  {
    nullableOfNullable
    ...ListFragmentInner
  }
`)
