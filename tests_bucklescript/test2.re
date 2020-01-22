module MyQuery = [%graphql
  {|
  query ($query: String!) {
    argNamedQuery(query: $query)
  }
|}
];

Jest.(
  describe("Argument named 'query'", () => {
    open Expect;
    open! Expect.Operators;

    test("Can create variables", () =>
      expect(MyQuery.makeVariables(~query="a query", ()))
      == Js.Json.parseExn({|{"query": "a query"}|})
    );
  })
);
