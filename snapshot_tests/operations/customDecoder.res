module StringOfInt = {
  let parse = string_of_int
  let serialize = int_of_string
  type t = string
}
module IntOfString = {
  let parse = int_of_string
  let serialize = string_of_int
  type t = int
}
module MyQuery = %graphql(`
  {
    variousScalars {
      string @ppxDecoder(module: "IntOfString")
      int @ppxDecoder(module: "StringOfInt")
    }
  }
`)
