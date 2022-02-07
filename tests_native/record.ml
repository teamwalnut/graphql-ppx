open Test_shared

type scalars = { string : string; int : int }

module Scalars = struct
  let pp formatter (obj : scalars) =
    Format.fprintf formatter "{ string = %a ; int = %a }" Format.pp_print_string
      obj.string Format.pp_print_int obj.int

  let equal (a : scalars) (b : scalars) = a.string = b.string && a.int = b.int
end

module MyQuery =
[%graphql
{|
  {
    variousScalars @ppxAs(type: "scalars") {
      string
      int
    }
  }
|}]

type qt = MyQuery.t

module MyQueryTest = struct
  let pp formatter (obj : qt) =
    Format.fprintf formatter "< variousScalars = @[%a@] >" Scalars.pp
      obj.variousScalars

  let equal (a : qt) (b : qt) = Scalars.equal a.variousScalars b.variousScalars
end
;;

[%graphql
  {|
  fragment ExternalFragment on VariousScalars @ppxAs(type: "scalars") {
    string
    int
  }

  query ExternalFragmentQuery{
    variousScalars {
      ...ExternalFragment
    }
  }
|}]

module ExternalFragmentQueryTest = struct
  let pp fmt (obj : ExternalFragmentQuery.t) =
    Format.fprintf fmt "< variousScalars = @[%a@] >" Scalars.pp
      obj.variousScalars

  let equal (a : ExternalFragmentQuery.t) (b : ExternalFragmentQuery.t) =
    Scalars.equal a.variousScalars b.variousScalars
end

module InlineFragmentQuery =
[%graphql
{|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}]

type if_qt = InlineFragmentQuery.t

module InlineFragmentQueryTest = struct
  let pp formatter (obj : if_qt) =
    Format.fprintf formatter "< dogOrHuman = @[%a@] >"
      (fun formatter v ->
        match v with
        | `FutureAddedValue _ -> Format.fprintf formatter "`FutureAddedValue(_)"
        | `Dog (dog : InlineFragmentQuery.t_dogOrHuman_Dog) ->
          Format.fprintf formatter "`Dog @[<>< name = %a ; barkVolume = %a >@]"
            Format.pp_print_string dog.name Format.pp_print_float dog.barkVolume)
      obj.dogOrHuman

  let equal (a : if_qt) (b : if_qt) =
    match (a.dogOrHuman, b.dogOrHuman) with
    | `Dog a, `Dog b -> a.name = b.name
    | `FutureAddedValue a, `FutureAddedValue b -> a = b
    | _ -> false
end
;;

[%graphql
  {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }

  query UnionExternalFragmentQuery {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}]

module UnionExternalFragmentQueryTest = struct
  let pp fmt (obj : UnionExternalFragmentQuery.t) =
    Format.fprintf fmt "< dogOrHuman = @[%a@] >"
      (fun formatter v ->
        match v with
        | `FutureAddedValue _ -> Format.fprintf formatter "`FutureAddedValue(_)"
        | `Dog (dog : DogFragment.t) ->
          Format.fprintf formatter "`Dog @[<>< name = %a ; barkVolume = %a >@]"
            Format.pp_print_string dog.name Format.pp_print_float dog.barkVolume)
      obj.dogOrHuman

  let equal (a : UnionExternalFragmentQuery.t)
    (b : UnionExternalFragmentQuery.t) =
    match (a.dogOrHuman, b.dogOrHuman) with
    | `Dog a, `Dog b -> a.name = b.name
    | `FutureAddedValue a, `FutureAddedValue b -> a = b
    | _ -> false
end

let decodes_record_in_selection () =
  test_exp
    ({| {"variousScalars": {"string": "a string", "int": 123}} |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    { variousScalars = { string = "a string"; int = 123 } }
    MyQueryTest.equal MyQueryTest.pp

let decodes_record_in_external_fragment () =
  test_exp
    ({| {"variousScalars": {"string": "a string", "int": 123}} |}
   |> Yojson.Basic.from_string |> ExternalFragmentQuery.unsafe_fromJson
   |> ExternalFragmentQuery.parse)
    { variousScalars = { string = "a string"; int = 123 } }
    ExternalFragmentQueryTest.equal ExternalFragmentQueryTest.pp

let decodes_record_in_inline_fragment () =
  test_exp
    ({| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
   |> Yojson.Basic.from_string |> InlineFragmentQuery.unsafe_fromJson
   |> InlineFragmentQuery.parse)
    { dogOrHuman = `Dog { name = "name"; barkVolume = 123.0 } }
    InlineFragmentQueryTest.equal InlineFragmentQueryTest.pp

let decodes_record_in_external_fragment_on_union_selections () =
  test_exp
    ({| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
   |> Yojson.Basic.from_string |> UnionExternalFragmentQuery.unsafe_fromJson
   |> UnionExternalFragmentQuery.parse)
    { dogOrHuman = `Dog { name = "name"; barkVolume = 123.0 } }
    UnionExternalFragmentQueryTest.equal UnionExternalFragmentQueryTest.pp

let tests =
  [
    ("Decodes a record in a selection", decodes_record_in_selection);
    ( "Decodes a record in an external fragment",
      decodes_record_in_external_fragment );
    ("Decodes a record in an inline fragment", decodes_record_in_inline_fragment);
    ( "Decodes a record in an external fragment on union selections",
      decodes_record_in_external_fragment_on_union_selections );
  ]
