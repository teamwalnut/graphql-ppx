module StringOfInt = {
  let parse = Int.toString
  let serialize = Int.fromString
  type t = string
}
module IntOfString = {
  let parse = Int.fromString
  let serialize = Int.toString
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
