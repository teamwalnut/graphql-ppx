/* TODO: fix */
/* module QueryWithFragments = [%graphql
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
]; */
