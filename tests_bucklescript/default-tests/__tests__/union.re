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

open Jest;
open Expect;

let json = {| {
  "dogOrHuman": {
    "__typename": "Dog",
    "name": "Fido",
    "barkVolume": 123
  }
} |};

describe("Union types", () => {
  test("Decodes exhaustive query", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({"dogOrHuman": `Dog({"name": "Fido", "barkVolume": 123.0})})
  );
});
