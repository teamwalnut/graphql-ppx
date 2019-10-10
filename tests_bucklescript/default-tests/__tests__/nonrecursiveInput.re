module MyQuery = [%graphql
  {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg)
    }
  |}
];

open Jest;
open Expect;

describe("Recursive input types", () =>
  test("Constructing a recursive input type", () => {
    let variables =
      MyQuery.make(~arg={"field": Some("test"), "enum": Some(`SECOND)}, ())##variables;
    let json = {| {
          "arg": {
            "field": "test",
            "enum": "SECOND"
          }
        } |};

    variables |> expect |> toEqual(Js.Json.parseExn(json));
  })
);
