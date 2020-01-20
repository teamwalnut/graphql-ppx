module MyQuery = [%graphql
  {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg) # comment to test
    }
  |}
];

Jest.(
  describe("Comment in query", () => {
    open Expect;
    open! Expect.Operators;

    test("Constructs with comment in query", () =>
      expect(
        MyQuery.makeVariables(
          ~arg={"field": Some("test"), "enum": Some(`SECOND)},
          (),
        ),
      )
      == Js.Json.parseExn(
           {| {
          "arg": {
            "field": "test",
            "enum": "SECOND"
          }
        } |},
         )
    );
  })
);
