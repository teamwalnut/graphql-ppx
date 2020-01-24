module RemoveFamilyMemberMutation = [%graphql
  {|
  mutation RemoveFamilyMember($id: ID!) {
    removeFamilyMember(id: $id) {
      id
      members {
        id
      }
    }
  }
|}
];
