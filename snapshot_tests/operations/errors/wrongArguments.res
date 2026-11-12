%graphql(`
  fragment FragmentWithArgs on Lists @argumentDefinitions(arg1: {type: "String"}) {
    listWithArg(arg1: $arg1)
  }
`)

%graphql(`
  query MyQuery {
    lists {
      nonNullableOfNullable
      ...FragmentWithArgs
    }
  }
  `)
