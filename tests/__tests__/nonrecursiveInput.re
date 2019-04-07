module MyQuery = [%graphql
  {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg)
    }
  |}
];



Jest.(
  describe("Recursive input types", () => {
    open Expect;

    test("Constructing a recursive input type", () => {
        let variables = MyQuery.make(
          ~arg={"field": Some("test"), "enum": Some(`SECOND)},
          (),
        )##variables;
        let json = {| {
          "arg": {
            "field": "test",
            "enum": "SECOND"
          }
        } |};

        variables
        |> expect
        |> toEqual(Js.Json.parseExn(json))
    });
  })
);
