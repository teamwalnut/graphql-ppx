module GraphQL_PPX = {
  // mock
  let deepMerge = (json1, _) => {
    json1;
  };
};

[%graphql
  {|
  fragment InterfaceFragment on User {
    id
  }

  fragment AnotherFragment on User {
    id
    ...InterfaceFragment
  }

  fragment AnonUser on AnonymousUser {
    id
    ...InterfaceFragment
  }
|}
];
