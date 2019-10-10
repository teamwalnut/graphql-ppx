module MyQuery = [%graphql
  {|
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
      enum
    }
  }
|}
];

Jest.(
  describe("Scalars", () => {
    open Expect;

    test("Decodes non-null scalars", () => {
      let json = {| {
        "variousScalars": {
          "nullableString": "a nullable string",
          "string": "a string",
          "nullableInt": 456,
          "int": 123,
          "nullableFloat": 678.5,
          "float": 1234.5,
          "nullableBoolean": false,
          "boolean": true,
          "nullableID": "a nullable ID",
          "id": "an ID",
          "enum": "FIRST"
        }
      } |};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "variousScalars": {
             "nullableString": Some("a nullable string"),
             "string": "a string",
             "nullableInt": Some(456),
             "int": 123,
             "nullableFloat": Some(678.5),
             "float": 1234.5,
             "nullableBoolean": Some(false),
             "boolean": true,
             "nullableID": Some("a nullable ID"),
             "id": "an ID",
             "enum": `FIRST,
           },
         });
    });

    test("Decodes null scalars", () => {
      let json = {| {
        "variousScalars": {
          "nullableString": null,
          "string": "a string",
          "nullableInt": null,
          "int": 123,
          "nullableFloat": null,
          "float": 1234.5,
          "nullableBoolean": null,
          "boolean": true,
          "nullableID": null,
          "id": "an ID",
          "enum": "FIRST"
        }
      } |};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "variousScalars": {
             "nullableString": None,
             "string": "a string",
             "nullableInt": None,
             "int": 123,
             "nullableFloat": None,
             "float": 1234.5,
             "nullableBoolean": None,
             "boolean": true,
             "nullableID": None,
             "id": "an ID",
             "enum": `FIRST,
           },
         });
    });

    test("Decodes omitted scalars", () => {
      let json = {| {
        "variousScalars": {
          "string": "a string",
          "int": 123,
          "float": 1234.5,
          "boolean": true,
          "id": "an ID",
          "enum": "FIRST"
        }
      } |};

      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({
           "variousScalars": {
             "nullableString": None,
             "string": "a string",
             "nullableInt": None,
             "int": 123,
             "nullableFloat": None,
             "float": 1234.5,
             "nullableBoolean": None,
             "boolean": true,
             "nullableID": None,
             "id": "an ID",
             "enum": `FIRST,
           },
         });
    });
  })
);
