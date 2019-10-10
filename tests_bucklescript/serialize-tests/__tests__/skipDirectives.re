module MyQuery = [%graphql
  {|
  query ($var: Boolean!) {
    v1: variousScalars {
      nullableString @skip(if: $var)
      string @skip(if: $var)
    }
    v2: variousScalars {
      nullableString @include(if: $var)
      string @include(if: $var)
    }
  }
|}
];

Jest.(
  describe("Skip/include directives", () => {
    open Expect;
    open! Expect.Operators;

    test("Correct serialization", () => {
      let json = {|{"v1": {"nullableString": null, "string": null}, "v2": {"nullableString": null, "string": null}}|};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> MyQuery.serialize
      |> Js.Json.stringify
      |> expect
      |> toEqual(json |> Utils.whitespaceAgnostic);
    });
  })
);
