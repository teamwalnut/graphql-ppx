type scalars = { string : string; int : int }

let scalars =
  (module struct
    type t = scalars

    let pp formatter (obj : scalars) =
      Format.fprintf formatter
        ("{ string = %a ; int = %a }"
        [@reason.raw_literal "{ string = %a ; int = %a }"])
        Format.pp_print_string obj.string Format.pp_print_int obj.int

    let equal (a : scalars) (b : scalars) = a.string = b.string && a.int = b.int
  end : Alcotest.TESTABLE
    with type t = scalars)

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

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< variousScalars = @[%a@] >"
        [@reason.raw_literal "< variousScalars = @[%a@] >"])
        (Alcotest.pp scalars) obj.variousScalars

    let equal (a : qt) (b : qt) =
      Alcotest.equal scalars a.variousScalars b.variousScalars
  end : Alcotest.TESTABLE
    with type t = qt)
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

let external_fragment_query =
  (module struct
    type t = ExternalFragmentQuery.t

    let pp formatter (obj : ExternalFragmentQuery.t) =
      Format.fprintf formatter
        ("< variousScalars = @[%a@] >"
        [@reason.raw_literal "< variousScalars = @[%a@] >"])
        (Alcotest.pp scalars) obj.variousScalars

    let equal (a : ExternalFragmentQuery.t) (b : ExternalFragmentQuery.t) =
      Alcotest.equal scalars a.variousScalars b.variousScalars
  end : Alcotest.TESTABLE
    with type t = ExternalFragmentQuery.t)

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

let inline_fragment_query =
  (module struct
    type t = if_qt

    let pp formatter (obj : if_qt) =
      Format.fprintf formatter
        ("< dogOrHuman = @[%a@] >"
        [@reason.raw_literal "< dogOrHuman = @[%a@] >"])
        (fun formatter v ->
          match v with
          | `FutureAddedValue _ ->
            Format.fprintf formatter
              ("`FutureAddedValue" [@reason.raw_literal "`FutureAddedValue"])
          | `Dog (dog : InlineFragmentQuery.t_dogOrHuman_Dog) ->
            Format.fprintf formatter
              ("`Dog @[<>< name = %a ; barkVolume = %a >@]"
              [@reason.raw_literal "`Dog @[<>< name = %a ; barkVolume = %a >@]"])
              Format.pp_print_string dog.name Format.pp_print_float
              dog.barkVolume)
        obj.dogOrHuman

    let equal (a : if_qt) (b : if_qt) =
      match (a.dogOrHuman, b.dogOrHuman) with
      | `Dog a, `Dog b -> a.name = b.name
      | `FutureAddedValue a, `FutureAddedValue b -> a = b
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = if_qt)
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

let union_external_fragment_query =
  (module struct
    type t = UnionExternalFragmentQuery.t

    let pp formatter (obj : UnionExternalFragmentQuery.t) =
      Format.fprintf formatter
        ("< dogOrHuman = @[%a@] >"
        [@reason.raw_literal "< dogOrHuman = @[%a@] >"])
        (fun formatter v ->
          match v with
          | `FutureAddedValue _ ->
            Format.fprintf formatter
              ("`FutureAddedValue" [@reason.raw_literal "`FutureAddedValue"])
          | `Dog (dog : DogFragment.t) ->
            Format.fprintf formatter
              ("`Dog @[<>< name = %a ; barkVolume = %a >@]"
              [@reason.raw_literal "`Dog @[<>< name = %a ; barkVolume = %a >@]"])
              Format.pp_print_string dog.name Format.pp_print_float
              dog.barkVolume)
        obj.dogOrHuman

    let equal (a : UnionExternalFragmentQuery.t)
      (b : UnionExternalFragmentQuery.t) =
      match (a.dogOrHuman, b.dogOrHuman) with
      | `Dog a, `Dog b -> a.name = b.name
      | `FutureAddedValue a, `FutureAddedValue b -> a = b
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = UnionExternalFragmentQuery.t)

let decodes_record_in_selection () =
  Alcotest.check my_query
    ("query result equality" [@reason.raw_literal "query result equality"])
    ({| {"variousScalars": {"string": "a string", "int": 123}} |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      variousScalars =
        { string = ("a string" [@reason.raw_literal "a string"]); int = 123 };
    }

let decodes_record_in_external_fragment () =
  Alcotest.check external_fragment_query
    ("query result equality" [@reason.raw_literal "query result equality"])
    ({| {"variousScalars": {"string": "a string", "int": 123}} |}
   |> Yojson.Basic.from_string |> ExternalFragmentQuery.unsafe_fromJson
   |> ExternalFragmentQuery.parse)
    {
      variousScalars =
        { string = ("a string" [@reason.raw_literal "a string"]); int = 123 };
    }

let decodes_record_in_inline_fragment () =
  Alcotest.check inline_fragment_query
    ("query result equality" [@reason.raw_literal "query result equality"])
    ({| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
   |> Yojson.Basic.from_string |> InlineFragmentQuery.unsafe_fromJson
   |> InlineFragmentQuery.parse)
    {
      dogOrHuman =
        `Dog
          { name = ("name" [@reason.raw_literal "name"]); barkVolume = 123.0 };
    }

let decodes_record_in_external_fragment_on_union_selections () =
  Alcotest.check union_external_fragment_query
    ("query result equality" [@reason.raw_literal "query result equality"])
    ({| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
   |> Yojson.Basic.from_string |> UnionExternalFragmentQuery.unsafe_fromJson
   |> UnionExternalFragmentQuery.parse)
    {
      dogOrHuman =
        `Dog
          { name = ("name" [@reason.raw_literal "name"]); barkVolume = 123.0 };
    }

let tests =
  [
    ( ("Decodes a record in a selection"
      [@reason.raw_literal "Decodes a record in a selection"]),
      `Quick,
      decodes_record_in_selection );
    ( ("Decodes a record in an external fragment"
      [@reason.raw_literal "Decodes a record in an external fragment"]),
      `Quick,
      decodes_record_in_external_fragment );
    ( ("Decodes a record in an inline fragment"
      [@reason.raw_literal "Decodes a record in an inline fragment"]),
      `Quick,
      decodes_record_in_inline_fragment );
    ( ("Decodes a record in an external fragment on union selections"
      [@reason.raw_literal
        "Decodes a record in an external fragment on union selections"]),
      `Quick,
      decodes_record_in_external_fragment_on_union_selections );
  ]
