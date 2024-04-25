%graphql(`
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
`)

module WrongFragmentName = ListFragment

%graphql(`
  query MyQuery {
    lists {
      ...WrongFragmentName

    }
  }
  `)
