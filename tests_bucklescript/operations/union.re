module MyQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
      ...on Human {
        name
      }
    }
  }
|}
];

module MyQueryNoError = [%graphql
  {|
  {
    dogOrHuman {
      # this is valid graphql and should pass
      __typename
      ...on Dog {
        name
        barkVolume
      }
      ...on Human {
        name
      }
    }
  }
|}
];

type named = {
  name: string,
  __typename: string,
};

module NamedQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog @ppxAs(type: "named") {
        name
        __typename
      }
      ...on Human @ppxAs(type: "named") {
        name
        __typename
      }
    }
  }
|}
];

module NamedSpread = [%graphql
  {|
  fragment DogFields on Dog @ppxAs(type: "named") {
    name
    __typename
  }

  query dogOrHuman {
    dogOrHuman {
      ...on Dog {
        ...DogFields
      }
    }
  }
|}
];
