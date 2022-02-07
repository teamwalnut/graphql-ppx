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

let pp formatter (obj : qt) =
  Format.fprintf formatter "< mutationWithError = %a >"
    (fun formatter -> function
      | `Value (v : MyQuery.t_mutationWithError_value) ->
        Format.fprintf formatter "`Value @[<>< stringField = %a >@]"
          Format.pp_print_string v.stringField
      | `Errors (v : MyQuery.t_mutationWithError_errors array) ->
        Format.fprintf formatter "`Errors %a"
          (print_array
             (fun formatter (v : MyQuery.t_mutationWithError_errors) ->
             Format.fprintf formatter "< field = %a ; message = %a >"
               Format.pp_print_string
               (match v.field with
               | `FIRST -> "FIRST"
               | `SECOND -> "SECOND"
               | `THIRD -> "THIRD"
               | `FutureAddedValue _ -> "THIRD")
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

let converts_into_variant () =
  test_exp
    ({| {
    "mutationWithError": {
      "value": {
        "stringField": "a string"
      }
    }
  } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    { mutationWithError = `Value { stringField = "a string" } }
    equal pp

let tests = [ ("Converts object into variant", converts_into_variant) ]
