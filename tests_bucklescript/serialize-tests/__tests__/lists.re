module MyQuery = [%graphql
  {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }
|}
];

open Jest;
open Expect;

let json = {|
{
  "lists": {
    "nullableOfNullable": [null, "123"],
    "nullableOfNonNullable": null,
    "nonNullableOfNullable": [null, "123"],
    "nonNullableOfNonNullable": ["a", "b"]
  }
}
|};

describe("Lists", () =>
  test("Serializes correctly", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> MyQuery.serialize
    |> Js.Json.stringify
    |> Utils.whitespaceAgnostic
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic)
  )
);
