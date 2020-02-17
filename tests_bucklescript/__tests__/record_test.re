open Jest;
open Expect;

describe("Record", () => {
  test("Decodes a record in a selection", () => {
    {|{"variousScalars": {"string": "a string", "int": 123}}|}
    |> Js.Json.parseExn
    |> Record.MyQuery.parse
    |> expect
    |> toEqual(
         Record.MyQuery.{
           variousScalars: {
             string: "a string",
             int: 123,
           },
         },
       )
  });

  test("Decodes a record with one field in a selection", () => {
    {|{"variousScalars": {"nullableString": "a string"}}|}
    |> Js.Json.parseExn
    |> Record.OneFieldQuery.parse
    |> expect
    |> toEqual(
         Record.OneFieldQuery.{
           variousScalars: {
             nullableString: Some("a string"),
           },
         },
       )
  });

  test("Decodes a record in an external fragment", () => {
    {|{"variousScalars": {"string": "a string", "int": 123}}|}
    |> Js.Json.parseExn
    |> Record.ExternalFragmentQuery.Untitled1.parse
    |> expect
    |> toEqual(
         Record.ExternalFragmentQuery.{
           Untitled1.variousScalars: Fragment.{string: "a string", int: 123},
         },
       )
  });

  test("Decodes a record in an inline fragment", () => {
    {|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|}
    |> Js.Json.parseExn
    |> Record.InlineFragmentQuery.parse
    |> expect
    |> toEqual(
         Record.InlineFragmentQuery.{
           dogOrHuman: `Dog({name: "name", barkVolume: 123.0}),
         },
       )
  });

  test("Decodes a record in an external fragment on union selections", () => {
    {|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|}
    |> Js.Json.parseExn
    |> Record.UnionExternalFragmentQuery.Untitled1.parse
    |> expect
    |> toEqual(
         Record.UnionExternalFragmentQuery.{
           Untitled1.dogOrHuman:
             `Dog(DogFragment.{name: "name", barkVolume: 123.0}),
         },
       )
  });
});
