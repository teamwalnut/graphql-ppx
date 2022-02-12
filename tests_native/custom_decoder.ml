open Test_shared

module IntOfString = struct
  let parse = int_of_string
  let serialize = string_of_int

  type t = int
end

module StringOfInt = struct
  let parse = string_of_int
  let serialize = int_of_string

  type t = string
end

module My_query =
[%graphql
{|
  {
    variousScalars {
      string @ppxDecoder(module: "IntOfString")
      int @ppxDecoder(module: "StringOfInt")
    }
  }
|}]

type t = My_query.t

let equal_payload (a : t) (b : t) =
  a.variousScalars.string = b.variousScalars.string
  && a.variousScalars.int = b.variousScalars.int

let payload_to_json p = p |> My_query.serialize |> My_query.toJson

let test_payload (a : t) (b : t) =
  test_json_ (payload_to_json a) (payload_to_json b)

let runs_the_decoder () =
  test_payload
    (Json.Read.from_string {|{"variousScalars": {"string": "123", "int": 456}}|}
    |> My_query.unsafe_fromJson |> My_query.parse)
    { variousScalars = { string = 123; int = "456" } }

let tests = [ ("Runs the decoder", runs_the_decoder) ]
