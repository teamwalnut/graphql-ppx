module MyQuery =
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

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter "< dogOrHuman = %a >"
        (fun formatter -> function
          | `Dog (dog : MyQuery.t_dogOrHuman_Dog) ->
            Format.fprintf formatter
              "`Dog @[<>< name = %a ; barkVolume = %a >@]"
              Format.pp_print_string dog.name Format.pp_print_float
              dog.barkVolume
          | `FutureAddedValue _ -> Format.fprintf formatter "`FutureAddedValue")
        obj.dogOrHuman

    let equal (a : qt) (b : qt) =
      match (a.dogOrHuman, b.dogOrHuman) with
      | `Dog a, `Dog b -> a.name = b.name && a.barkVolume = b.barkVolume
      | `FutureAddedValue a, `FutureAddedValue b -> a = b
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = qt)

let decodes_non_exhaustive_query () =
  Alcotest.check my_query "result equality"
    ({| {
      "dogOrHuman": {
        "__typename": "Human",
        "name": "Max"
      }
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      dogOrHuman =
        `FutureAddedValue
          (`Assoc [ ("__typename", `String "Human"); ("name", `String "Max") ]);
    }

let tests =
  [ ("Decodes non-exhaustive query", `Quick, decodes_non_exhaustive_query) ]
