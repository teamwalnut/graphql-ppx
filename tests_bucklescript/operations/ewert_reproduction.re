module UserData = [%graphql
  {|
    fragment UserData on Workspace @argumentDefinitions(userId: {type: "String!"}) {
      user(id: $userId) {
        id
        firstname
        lastname
      }
    }
  |}
];

module Query = [%graphql
  {|
    query WorkspaceQuery($userId: String!) {
      workspace(id: "123") {
        ...UserData @arguments(userId: $userId)
      }
    }
  |}
];
