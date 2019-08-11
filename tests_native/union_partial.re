module MyQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}
];

type qt = {
  .
  dogOrHuman: [
    | `Dog(
        {
          .
          name: string,
          barkVolume: float,
        },
      )
    | `Nonexhaustive
  ],
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< dogOrHuman = %a >",
         formatter =>
           fun
           | `Dog(dog) =>
             Format.fprintf(
               formatter,
               "`Dog @[<>< name = %a ; barkVolume = %a >@]",
               Format.pp_print_string,
               dog#name,
               Format.pp_print_float,
               dog#barkVolume,
             )
           | `Nonexhaustive => Format.fprintf(formatter, "`Nonexhaustive"),
         obj#dogOrHuman,
       );

     let equal = (a: qt, b: qt) =>
       switch (a#dogOrHuman, b#dogOrHuman) {
       | (`Dog(a), `Dog(b)) =>
         a#name == b#name && a#barkVolume == b#barkVolume
       | (`Nonexhaustive, `Nonexhaustive) => true
       | _ => false
       };
   });

let decodes_non_exhaustive_query = () =>
  Alcotest.check(
    my_query,
    "result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
      "dogOrHuman": {
        "__typename": "Human",
        "name": "Max"
      }
    } |},
      ),
    ),
    {as _; pub dogOrHuman = `Nonexhaustive},
  );

let tests = [
  ("Decodes non-exhaustive query", `Quick, decodes_non_exhaustive_query),
];
