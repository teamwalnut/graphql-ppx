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

type qt = My_query.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : t) =
      Format.fprintf formatter
        ("<variousScalars = @[<>string = @[%i@]; int = @[%s@]>@] >"
        [@reason.raw_literal
          "<variousScalars = @[<>string = @[%i@]; int = @[%s@]>@] >"])
        obj.variousScalars.string obj.variousScalars.int

    let equal (a : t) (b : t) =
      a.variousScalars.string = b.variousScalars.string
      && a.variousScalars.int = b.variousScalars.int
  end : Alcotest.TESTABLE
    with type t = qt)

let runs_the_decoder () =
  Alcotest.check my_query "query result equality"
    (Yojson.Basic.from_string
       {|{"variousScalars": {"string": "123", "int": 456}}|}
    |> My_query.unsafe_fromJson |> My_query.parse)
    { variousScalars = { string = 123; int = "456" } }

let tests = [ ("Runs the decoder", `Quick, runs_the_decoder) ]
