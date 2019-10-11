type scalars = {
  string,
  int,
};

module MyQuery = [%graphql
  {|
  {
    variousScalars @bsRecord {
      string
      int
    }
  }
|}
];

open Jest;
open Expect;

describe("Record", () =>
  test("Encodes a record in a Selection", () => {
    let json = {|{"variousScalars": {"string": "a string", "int": 123}}|};

    json
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> MyQuery.serialize
    |> Js.Json.stringify
    |> expect
    |> toEqual(json |> Utils.whitespaceAgnostic);
  })
);
