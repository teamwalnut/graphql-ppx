module MyQuery = [%graphql
  {|
  query {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}
];

open Jest;
open Expect;

let json = {| {
  "dogOrHuman": {
    "__typename": "Human",
    "name": "Max"
  }
}|};

describe("Union types", () =>
  test("Decodes non-exhaustive query", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({"dogOrHuman": `Nonexhaustive})
  )
);
