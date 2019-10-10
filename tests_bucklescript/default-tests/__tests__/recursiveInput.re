module MyQuery = [%graphql
  {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}
];

open Jest;
open Expect;

describe("Recursive input types", () =>
  test("Constructing a recursive input type", () => {
    let json = {| {
        "arg": {
          "otherField": "test",
          "inner": {
            "otherField": "inner",
            "enum": "SECOND"
          }
        }
      } |};

    MyQuery.make(
      ~arg={
        "otherField": Some("test"),
        "enum": None,
        "inner":
          Some({
            "otherField": Some("inner"),
            "inner": None,
            "enum": Some(`SECOND),
          }),
      },
      (),
    )##variables
    |> expect
    |> toEqual(Js.Json.parseExn(json));
  })
);
