[%graphql
  {|
  fragment DogFragment on Dog {
    name
  }
  fragment HumanFragment on Human {
    name
  }

  query MyQuery {
    dogOrHuman {
      ... on Dog {
        ...DogFragment
      }
      ... on Human {
        ...HumanFragment
      }
    }
  }
|}]
