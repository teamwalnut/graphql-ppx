module StringOfInt = {
  let parse = Int.toString
  let serialize = value => Int.fromString(value)->Option.getOrThrow
  type t = string
}
module IntOfString = {
  type t = int
  let parse = (value): t => Int.fromString(value)->Option.getOrThrow
  let serialize = Int.toString
}
module MyQuery = %graphql(`
  {
    variousScalars {
      string @ppxDecoder(module: "IntOfString")
      int @ppxDecoder(module: "StringOfInt")
    }
  }
`)
