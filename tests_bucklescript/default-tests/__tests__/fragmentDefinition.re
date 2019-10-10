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

let json = {|{
  "l1": {"nullableOfNullable": ["a", null, "b"]},
  "l2": {"nullableOfNullable": ["a", null, "b"]}
}|};

describe("Fragment definition", () => {
  test("Decodes the fragment", () =>
    json
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
});
