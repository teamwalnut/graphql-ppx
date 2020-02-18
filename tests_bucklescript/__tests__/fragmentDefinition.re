type record = {
  nullableOfNullable: option(array(option(string))),
  nullableOfNonNullable: option(array(string)),
};

let concat = ({nullableOfNullable, nullableOfNonNullable}) => {
  let x =
    nullableOfNullable
    ->Belt.Option.getWithDefault([||])
    ->Belt.Array.keepMap(x => x);
  let y = nullableOfNonNullable->Belt.Option.getWithDefault([||]);

  Belt.Array.concat(x, y);
};

module Fragments = [%graphql
  {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  fragment concatFragment on Lists @bsRecord @bsDecoder(fn: "concat") {
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

    l3: lists {
      ...Fragments.ConcatFragment
    }
  }
|}
];

open Jest;
open Expect;

describe("Fragment definition", () => {
  let expectedObject = {
    "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
    "nullableOfNonNullable": None,
  };

  test("Decodes the fragment", () =>
    {|
      {
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]},
        "l3": {
          "nullableOfNullable": ["a", null, "b", null, "c"],
          "nullableOfNonNullable": ["d", "e"]
          }
      }|}
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({
         "l1": expectedObject,
         "l2": {
           "frag1": expectedObject,
           "frag2": expectedObject,
         },
         "l3": [|"a", "b", "c", "d", "e"|],
       })
  );

  test("Removes @bsField from query output", () =>
    MyQuery.query |> Js.String.includes("@bsField") |> expect |> toBe(false)
  );
});