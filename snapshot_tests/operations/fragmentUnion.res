module Graphql_ppx_runtime = {
  // mock
  let assign_typename: (
    Js.Json.t,
    string,
  ) => Js.Json.t = %raw(` (obj, typename) => { obj.__typename = typename; return obj } `)
}

%graphql(`
  fragment DogFragment on Dog {
    name
  }
  fragment HumanFragment on Human {
    name
  }

  query MyQuery {
    dogOrHuman {
      ... on Dog {
        ...DogFragment
      }
      ... on Human {
        ...HumanFragment
      }
    }
  }
`)
