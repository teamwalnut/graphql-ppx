module MyQuery = [%graphql
  {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}
];

Jest.(
  describe("Recursive input types", () => {
    open Expect;

    test("Constructing a recursive input type", () =>
        (MyQuery.make(
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
        )##variables)
        |> expect |> toEqual(
      Js.Json.parseExn(
           {| {
        "arg": {
          "otherField": "test",
          "inner": {
            "otherField": "inner",
            "enum": "SECOND"
          }
        }
      } |}
         ))
    );
  })
);
