module MyQuery = [%graphql
  {|
  {
    first: nestedObject {
      __typename
      inner {
        __typename
        inner {
          __typename
          field
        }
      }
    }
  }
|}
];

open Jest;
open Expect;

describe("Typename as implicit field", () =>
  test("Decodes typename as a non-nullable string", () => {
    let json = {|{"first": {"__typename": "NestedObject", "inner": null}}|};

    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({
         "first": {
           "__typename": "NestedObject",
           "inner": None,
         },
       });
  })
);
