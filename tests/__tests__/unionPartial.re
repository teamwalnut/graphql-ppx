open Jest;







    open Expect;

module MyQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}
];


  describe("Union types", () => {


    test("Decodes non-exhaustive query", () => {
    let json = {| {
      "dogOrHuman": {
        "__typename": "Human",
        "name": "Max"
      }
    } |};

    json 
    |> Js.Json.parseExn
    |> MyQuery.parse
    |> expect
    |> toEqual({"dogOrHuman": `Nonexhaustive});





    });
  });

