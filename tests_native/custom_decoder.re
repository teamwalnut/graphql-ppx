module IntOfString = {
  let parse = int_of_string;
  type t = int;
};

module StringOfInt = {
  let parse = string_of_int;
  type t = string;
};



module My_query = [%graphql
  {|
  {
    variousScalars {
      string @decoder(module: "IntOfString")
      int @decoder(module: "StringOfInt")
    }
  }
|}
];

type qt = {
  .
  variousScalars: {
    .
    string: int,
    int: string,
  },
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj) =>
       Format.fprintf(
         formatter,
         "<variousScalars = @[<>string = @[%i@]; int = @[%s@]>@] >",
         obj#variousScalars#string,
         obj#variousScalars#int,
       );

     let equal = (a, b) =>
       a#variousScalars#string == b#variousScalars#string
       && a#variousScalars#int == b#variousScalars#int;
   });

let runs_the_decoder = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    My_query.parse(
      Yojson.Basic.from_string(
        {|{"variousScalars": {"string": "123", "int": 456}}|},
      ),
    ),
    {as _; pub variousScalars = {as _; pub string = 123; pub int = "456"}},
  );

let tests = [("Runs the decoder", `Quick, runs_the_decoder)];
