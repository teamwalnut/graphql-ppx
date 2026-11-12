module UserData = %graphql(`
    fragment UserData on Workspace @argumentDefinitions(userId: {type: "String!"}) {
      user(id: $userId) {
        id
        firstname
        lastname
      }
    }
  `)
