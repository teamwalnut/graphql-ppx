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

Jest.(
  describe("Lists", () => {
    open Expect;

    test("Null in nullable lists", () =>
      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "lists": {
             "nullableOfNullable": Some([|None, Some("123")|]),
             "nullableOfNonNullable": None,
             "nonNullableOfNullable": [|None, Some("123")|],
             "nonNullableOfNonNullable": [|"a", "b"|],
           },
         })
    );
  })
);
