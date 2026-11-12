open Test_shared

module MyQuery =
[%graphql
{|
  mutation {
    mutationWithError {
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

let printed_query () =
  match Str.search_forward (Str.regexp "^mutation") MyQuery.query 0 with
  | 0 -> Pass
  | (exception Not_found) | _ ->
    Fail "mutation not found as first index of the operation string"

let tests = [ ("Printed query is a mutation", printed_query) ]
