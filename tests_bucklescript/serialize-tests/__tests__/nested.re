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

open Jest;
open Expect;

let json = {|
  {
    "first": {"inner": {"inner": {"field": "second"}}},
    "second": {"inner": null}
  }
|};

describe("Nested", () =>
  test("Serializes", () =>
    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> MyQuery.serialize
    |> Js.Json.stringify
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic)
  )
);
