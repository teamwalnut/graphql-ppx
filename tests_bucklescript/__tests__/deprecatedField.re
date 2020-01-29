module MyQuery = [%graphql {|
  query {
    deprecatedField
  }
|}];

Jest.(
  describe("Deprecated field", () => {
    open Expect;
    open! Expect.Operators;

    test("Should trigger a compilation warning", () =>
      expect(MyQuery.parse(Js.Json.parseExn({| {"deprecatedField": ""} |})))
      == {"deprecatedField": ""}
    );
  })
);
