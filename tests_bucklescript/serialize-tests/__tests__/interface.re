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
  test("Serializes", () =>
    json
    |> Js.Json.parseExn
    |> QueryWithFragments.parse
    |> QueryWithFragments.serialize
    |> Js.Json.stringify
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic)
  );
});
