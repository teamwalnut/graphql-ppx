type record = {
  f1: string,
  f2: string,
};

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
        inner @bsRecord {
          f1: field
          f2: field
        }
      }
    }
  }
|}
];

let json = {|
  {
    "first": {"inner": {"inner": {"field": "second"}}},
    "second": {"inner": null}
  }
|};

open Jest;
open Expect;

describe("Nested", () =>
  test("Decodes recursively", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({
         "first": {
           "inner": Some({"inner": Some({"field": "second"})}),
         },
         "second": {
           "inner": None,
         },
       })
  )
);
