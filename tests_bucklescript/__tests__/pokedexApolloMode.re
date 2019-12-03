open Jest;
open Expect;

module MyQuery = [%graphql
  {|
    {
      pokemon(name: "Pikachu") {
        id
        name
      }
    }
  |};
  {schema: "../pokedex_schema.json"}
];

describe("Apollo mode with alternate schema", () =>
  test("Adds __typename to objects", () => {
    let typenameRegex = [%bs.re {|/__typename/g|}];
    MyQuery.query
    |> Js.String.match(typenameRegex)
    |> Belt.Option.map(_, Array.length)
    |> expect
    |> toEqual(Some(2));
  })
);
