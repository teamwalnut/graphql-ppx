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

let json_str = {|{ "dogOrHuman": { "__typename": "Dog", "name": "Woof" } }|}
let json = (Json.Read.from_string json_str)
let serialization_test () =
    Test_shared.test_json_ json (json |> MyQuery.unsafe_fromJson |> MyQuery.parse |> MyQuery.serialize |> MyQuery.toJson)

let tests = [ ("Null in nullable lists", serialization_test) ]
