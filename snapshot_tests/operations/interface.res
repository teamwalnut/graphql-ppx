module Graphql_ppx_runtime = {
  // mock
  let deepMerge = (json1, _) => json1
}

module QueryWithOnlyFragments = %graphql(`
    query {
      users {
        ... on AdminUser {
          id
          name
        }
        ... on AnonymousUser {
          id
          anonymousId
        }
      }
    }
  `)

module QueryWithTypename = %graphql(`
    query {
      users {
        ... on AdminUser {
          __typename
          id
        }
      }
    }
  `)

module QueryWithMixedFragments = %graphql(`
    query {
      users {
        id
        ... on AdminUser {
          name
        }
        ... on AnonymousUser {
          anonymousId
        }
      }
    }
   `)

module MixedQueryWithTypename = %graphql(`
    query {
      users {
        id
        __typename
        ... on AdminUser {
          __typename
          id
        }
      }
    }
  `)

module QueryWithoutFragments = %graphql(`
    query {
      users {
        id
      }
    }
  `)
