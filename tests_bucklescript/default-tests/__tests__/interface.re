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

open Jest;
open Expect;

let json = {|{
 "users": [
    { "__typename": "AdminUser", "id": "1", "name": "bob" },
    { "__typename": "AnonymousUser", "id": "2", "anonymousId": 1},
    { "__typename": "OtherUser", "id": "3"}
]}|};

describe("Interface definition", () => {
  test("Decodes the interface with fragments", () =>
    json
    |> Js.Json.parseExn
    |> QueryWithFragments.parse
    |> expect
    |> toEqual({
         "users": [|
           `AdminUser({"id": "1", "name": "bob"}),
           `AnonymousUser({"id": "2", "anonymousId": 1}),
           `User(("OtherUser", {"id": "3"})),
         |],
       })
  );

  test("Decodes the interface without fragments", () =>
    json
    |> Js.Json.parseExn
    |> QueryWithoutFragments.parse
    |> expect
    |> toEqual({
         "users": [|
           `User(("AdminUser", {"id": "1"})),
           `User(("AnonymousUser", {"id": "2"})),
           `User(("OtherUser", {"id": "3"})),
         |],
       })
  );
});
