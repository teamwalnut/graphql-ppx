module MyQuery =
[%graphql
{|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
      }
    }
  }
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< dogOrHuman = %a >" [@reason.raw_literal "< dogOrHuman = %a >"])
        (fun formatter -> function
          | `FutureAddedValue _ ->
            Format.fprintf formatter
              ("`FutureAddedValue" [@reason.raw_literal "`FutureAddedValue"])
          | `Dog (dog : MyQuery.t_dogOrHuman_Dog) ->
            Format.fprintf formatter
              ("`Dog @[<>< name = %a ; barkVolume = %a >@]"
              [@reason.raw_literal "`Dog @[<>< name = %a ; barkVolume = %a >@]"])
              Format.pp_print_string dog.name Format.pp_print_float
              dog.barkVolume
          | `Human (human : MyQuery.t_dogOrHuman_Human) ->
            Format.fprintf formatter
              ("`Human @[<>< name = %a >@]"
              [@reason.raw_literal "`Human @[<>< name = %a >@]"])
              Format.pp_print_string human.name)
        obj.dogOrHuman

    let equal (a : qt) (b : qt) =
      match (a.dogOrHuman, b.dogOrHuman) with
      | `Dog a, `Dog b -> a.name = b.name && a.barkVolume = b.barkVolume
      | `Human a, `Human b -> a.name = b.name
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = qt)

let decodes_exhaustive_query () =
  Alcotest.check my_query
    ("result equality" [@reason.raw_literal "result equality"])
    ({| {
      "dogOrHuman": {
        "__typename": "Dog",
        "name": "Fido",
        "barkVolume": 123.0
      }
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      dogOrHuman =
        `Dog
          { name = ("Fido" [@reason.raw_literal "Fido"]); barkVolume = 123.0 };
    }

let tests =
  [
    ( ("Decodes exhaustive query"
      [@reason.raw_literal "Decodes exhaustive query"]),
      `Quick,
      decodes_exhaustive_query );
  ]
