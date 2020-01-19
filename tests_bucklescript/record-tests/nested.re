module MyQuery = [%graphql
  {|
  {
    first: nestedObject {
      inner {
        inner {
          field
        }
      }
    }
    second: nestedObject {
      inner {
        inner {
          f1: field
          f2: field
        }
      }
    }
  }
|}
];

Jest.(
  describe("Nested", () => {
    open Expect;
    open! Expect.Operators;
    MyQuery.(
      test("Decodes recursively", () =>
        expect(
          parse(
            Js.Json.parseExn(
              {|
      {"first": {"inner": {"inner": {"field": "second"}}},
       "second": {"inner": null}}
    |},
            ),
          ),
        )
        == {
             first: {
               inner: Some({inner: Some({field: "second"})}),
             },
             second: {
               inner: None,
             },
           }
      )
    );
  })
);
