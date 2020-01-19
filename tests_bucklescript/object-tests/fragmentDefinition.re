module Fragments = [%graphql
  {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
|}
];

module MyQuery = [%graphql
  {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }
  }
|}
];

open Jest;
open Expect;

describe("Fragment definition", () => {
  test("Decodes the fragment", () =>
    {|
      {
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]}
      }|}
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({
         "l1": {
           "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
           "nullableOfNonNullable": None,
         },
         "l2": {
           "frag1": {
             "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
             "nullableOfNonNullable": None,
           },
           "frag2": {
             "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
             "nullableOfNonNullable": None,
           },
         },
       })
  );

  test("Removes @bsField from query output", () =>
    MyQuery.query |> Js.String.includes("@bsField") |> expect |> toBe(false)
  );
});
