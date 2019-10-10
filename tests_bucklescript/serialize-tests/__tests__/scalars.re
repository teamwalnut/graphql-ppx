module MyQuery = [%graphql
  {|
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
      enum
    }
  }
|}
];

open Jest;
open Expect;

describe("Scalars", () =>
  test("Serializes correctly", () => {
    let json = {| {
        "variousScalars": {
          "nullableString": null,
          "string": "a string",
          "nullableInt": null,
          "int": 123,
          "nullableFloat": null,
          "float": 1234.5,
          "nullableBoolean": null,
          "boolean": true,
          "nullableID": null,
          "id": "an ID",
          "enum": "FIRST"
        }
      } |};

    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> MyQuery.serialize
    |> Js.Json.stringify
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic);
  })
);
