open Test_shared;;

[%graphql
  {|
  query ImplicitTypename {
    variousScalars {
      id
      __typename
    }
    users {
      id
      __typename
    }
  }
|}]

let test () =
  let json =
    Json.Read.from_string
      {|{ "variousScalars": { "id": "123", "__typename": "VariousScalars" }, "users": {"id": "123", "__typename": "User"} }|}
  in
  test_json_ json
    (ImplicitTypename.unsafe_fromJson json |> ImplicitTypename.toJson)

let tests = [ ("Accepts __typename", test) ]
