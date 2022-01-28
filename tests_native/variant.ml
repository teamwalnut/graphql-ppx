open Test_shared

module MyQuery =
[%graphql
{|
  mutation {
    mutationWithError @bsVariant {
      value {
        stringField
      }

      errors {
        field
        message
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
        ("< mutationWithError = %a >"
        [@reason.raw_literal "< mutationWithError = %a >"])
        (fun formatter -> function
          | `Value (v : MyQuery.t_mutationWithError_value) ->
            Format.fprintf formatter
              ("`Value @[<>< stringField = %a >@]"
              [@reason.raw_literal "`Value @[<>< stringField = %a >@]"])
              Format.pp_print_string v.stringField
          | `Errors (v : MyQuery.t_mutationWithError_errors array) ->
            Format.fprintf formatter
              ("`Errors %a" [@reason.raw_literal "`Errors %a"])
              (print_array
                 (fun formatter (v : MyQuery.t_mutationWithError_errors) ->
                 Format.fprintf formatter
                   ("< field = %a ; message = %a >"
                   [@reason.raw_literal "< field = %a ; message = %a >"])
                   Format.pp_print_string
                   (match v.field with
                   | `FIRST -> ("FIRST" [@reason.raw_literal "FIRST"])
                   | `SECOND -> ("SECOND" [@reason.raw_literal "SECOND"])
                   | `THIRD -> ("THIRD" [@reason.raw_literal "THIRD"])
                   | `FutureAddedValue _ ->
                     ("THIRD" [@reason.raw_literal "THIRD"]))
                   Format.pp_print_string v.message))
              v)
        obj.mutationWithError

    let equal (a : qt) (b : qt) =
      match (a.mutationWithError, b.mutationWithError) with
      | `Value a, `Value b -> a.stringField = b.stringField
      | `Errors a, `Errors b ->
        array_zipmap
          (fun (a : MyQuery.t_mutationWithError_errors)
             (b : MyQuery.t_mutationWithError_errors) ->
            a.field = b.field && a.message = b.message)
          a b
        |> Array.to_list
        |> List.for_all (fun x -> x)
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = qt)

let converts_into_variant () =
  Alcotest.check my_query
    ("result equality" [@reason.raw_literal "result equality"])
    ({| {
    "mutationWithError": {
      "value": {
        "stringField": "a string"
      }
    }
  } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      mutationWithError =
        `Value { stringField = ("a string" [@reason.raw_literal "a string"]) };
    }

let tests =
  [
    ( ("Converts object into variant"
      [@reason.raw_literal "Converts object into variant"]),
      `Quick,
      converts_into_variant );
  ]
