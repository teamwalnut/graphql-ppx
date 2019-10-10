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

    test("Decodes a record in a Selection", () => {
      let json = {|{"variousScalars": {"string": "a string", "int": 123}}|};
      let expected = {string: "a string", int: 123};
      json
      |> Js.Json.parseExn
      |> MyQuery.parse
      |> expect
      |> toEqual({"variousScalars": expected});
    });

    test("Decodes a record in an external fragment", () => {
      let json = {|{"variousScalars": {"string": "a string", "int": 123}}|};
      let expected = {string: "a string", int: 123};

      json
      |> Js.Json.parseExn
      |> ExternalFragmentQuery.parse
      |> expect
      |> toEqual({"variousScalars": expected});
    });

    test("Decodes a record in an inline fragment", () => {
      let expected = `Dog({name: "name", barkVolume: 123.0});
      let json = {|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|};

      json
      |> Js.Json.parseExn
      |> InlineFragmentQuery.parse
      |> expect
      |> toEqual({"dogOrHuman": expected});
    });

    test("Decodes a record in an external fragment on union selections", () => {
      let expected = `Dog({name: "name", barkVolume: 123.0});
      let json = {|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|};
      json
      |> Js.Json.parseExn
      |> UnionExternalFragmentQuery.parse
      |> expect
      |> toEqual({"dogOrHuman": expected});
    });
  })
);
