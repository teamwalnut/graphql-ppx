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
  }
|}
];

open Jest;
open Expect;

let json = {|{
  "l1": {"nullableOfNullable": ["a", null, "b"]},
}|};

describe("Fragment definition", () =>
  test("Serializes", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> MyQuery.serialize
    |> Js.Json.stringify
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic)
  )
);
