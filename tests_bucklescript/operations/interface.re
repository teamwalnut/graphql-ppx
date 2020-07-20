module QueryWithOnlyFragments = [%graphql
  {|
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
  |}
];

module QueryWithMixedFragments = [%graphql
  {|
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
   |}
];

module QueryWithoutFragments = [%graphql
  {|
    query {
      users {
        id
      }
    }
  |}
];
