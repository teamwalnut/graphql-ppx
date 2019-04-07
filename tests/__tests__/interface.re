module QueryWithFragments = [%graphql
  {|
   query {
    users {
      id
      ... on AdminUser {
        name
      }
      ... on AnonymousUser {
        anonymousId
      }
    }
  }
|}
];

module QueryWithoutFragments = [%graphql
  {|
   query {
    users {
      id
    }
  }
|}
];

let json = {|{
 "users": [
    { "__typename": "AdminUser", "id": "1", "name": "bob" },
    { "__typename": "AnonymousUser", "id": "2", "anonymousId": 1},
    { "__typename": "OtherUser", "id": "3"}
]}|};

Jest.(
  describe("Interface definition", () => {
    open Expect;
  
    test("Decodes the interface with fragments", () =>
      json
      |> Js.Json.parseExn
      |> QueryWithFragments.parse
      |> expect
      |> toEqual({
           "users": [|
             `AdminUser({"id": "1", "name": "bob"}),
             `AnonymousUser({"id": "2", "anonymousId": 1}),
             `User({"id": "3"}),
           |],
         })
    );

    test("Decodes the interface without fragments", () =>
      json 
      |> Js.Json.parseExn
      |> QueryWithoutFragments.parse |> expect |> toEqual(
       {
           "users": [|
             `User({"id": "1"}),
             `User({"id": "2"}),
             `User({"id": "3"}),
           |],
         })
    );

     test("Serializes", () =>
      json
      |> Js.Json.parseExn
      |> QueryWithFragments.parse
      |> QueryWithFragments.serialize
      |> Js.Json.stringify
      |> expect
      |> toEqual(json |> Utils.whitespaceAgnostic)
    );
  })
);
