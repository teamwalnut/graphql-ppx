module GraphQL_PPX = {
  // mock
  let deepMerge = (json1, _) => {
    json1;
  };
};

module UserData = [%graphql
  {|
    fragment UserData on User {
      id
    }
  |};
];

module MyQuery = [%graphql
  {|
    query {
      admins {
        id
        ...UserData
      }
    }
  |}
];
