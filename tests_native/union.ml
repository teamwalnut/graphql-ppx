open Test_shared

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

let pp formatter (obj : qt) =
  Format.fprintf formatter "< dogOrHuman = %a >"
    (fun formatter -> function
      | `FutureAddedValue _ -> Format.fprintf formatter "`FutureAddedValue"
      | `Dog (dog : MyQuery.t_dogOrHuman_Dog) ->
        Format.fprintf formatter "`Dog @[<>< name = %a ; barkVolume = %a >@]"
          Format.pp_print_string dog.name Format.pp_print_float dog.barkVolume
      | `Human (human : MyQuery.t_dogOrHuman_Human) ->
        Format.fprintf formatter "`Human @[<>< name = %a >@]"
          Format.pp_print_string human.name)
    obj.dogOrHuman

let equal (a : qt) (b : qt) =
  match (a.dogOrHuman, b.dogOrHuman) with
  | `Dog a, `Dog b -> a.name = b.name && a.barkVolume = b.barkVolume
  | `Human a, `Human b -> a.name = b.name
  | _ -> false

let decodes_exhaustive_query () =
  test_exp
    ({| {
      "dogOrHuman": {
        "__typename": "Dog",
        "name": "Fido",
        "barkVolume": 123.0
      }
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    { dogOrHuman = `Dog { name = "Fido"; barkVolume = 123.0 } }
    equal pp

let tests = [ ("Decodes exhaustive query", decodes_exhaustive_query) ]
