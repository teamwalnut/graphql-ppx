type scalars = {
  string,
  int,
};

type dog = {
  name: string,
  barkVolume: float,
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

module ExternalFragmentQuery = [%graphql
  {|
  fragment Fragment on VariousScalars @bsRecord {
    string
    int
  }

  {
    variousScalars {
      ...Fragment
    }
  }
|}
];

module InlineFragmentQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog @bsRecord {
        name
        barkVolume
      }
    }
  }
|}
];

module UnionExternalFragmentQuery = [%graphql
  {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }

  {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}
];

Jest.(
  describe("Record", () => {
    open Expect;
    open! Expect.Operators;

    test("Encodes a record in a Selection", () => {
      let json = {|{"variousScalars": {"string": "a string", "int": 123}}|};

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
