module MyQuery = [%graphql
  {|
  mutation MyMutation($required: String!) {
    optionalInputArgs(required: $required, anotherRequired: "val")
  }
|}
];
