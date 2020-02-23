module StringOfInt = {
  let parse = string_of_int;
  type t = string;
};
module IntOfString = {
  let parse = int_of_string;
  type t = int;
};

module MyQuery = [%graphql
  {|
  {
    variousScalars {
      string @ppxDecoder(module: "IntOfString")
      int @ppxDecoder(module: "StringOfInt")
    }
  }
|}
];
