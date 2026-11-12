module Graphql_ppx_runtime = {
  // mock
  let deepMerge = (json1, _) => json1
}

%graphql(`
  fragment ListFragment on Lists {
    nullableOfNullable
  }

  fragment Another on Lists {
    nullableOfNullable
    ...ListFragment
  }
`)
