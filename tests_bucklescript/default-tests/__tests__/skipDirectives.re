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

    test("Responds with None to nulled fields", () => {
      let json = {|{"v1": {"nullableString": null, "string": null}, "v2": {"nullableString": null, "string": null}}|};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "v1": {
             "nullableString": None,
             "string": None,
           },
           "v2": {
             "nullableString": None,
             "string": None,
           },
         });
    });



    test("Responds with None to omitted fields", () => {
      let json = {|{"v1": {}, "v2": {}}|};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "v1": {
             "nullableString": None,
             "string": None,
           },
           "v2": {
             "nullableString": None,
             "string": None,
           },
         });
    });
  })
);
