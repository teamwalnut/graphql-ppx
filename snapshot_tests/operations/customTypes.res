module Color = {
  type t =
    | Red
    | Green
    | Blue
  let parse = json =>
    switch json->JSON.Decode.string {
    | Some("green") => Green
    | Some("blue") => Blue
    | Some("red")
    | Some(_)
    | None =>
      Red
    }
  let serialize = color =>
    switch color {
    | Red => "red"
    | Green => "green"
    | Blue => "blue"
    }->JSON.Encode.string
}

module NullableString = {
  type t =
    | Red
    | Green
    | Blue
  let parse: option<string> => t = color =>
    switch color {
    | Some("green") => Green
    | Some("blue") => Blue
    | Some("red") => Red
    | Some(_)
    | None =>
      Red
    }
  let serialize: t => option<string> = color =>
    switch color {
    | Red => Some("red")
    | Green => Some("green")
    | Blue => Some("blue")
    }
}

module DateTime = {
  type t = Date.t
  let parse = json =>
    switch json->JSON.Decode.string {
    | Some(str) => str->Date.fromString
    | None => Date.make()
    }
  let serialize = date => date->Date.toISOString->JSON.Encode.string
}

module MyQuery = %graphql(`
  query {
    customFields {
        currentTime
        favoriteColor
        futureTime
        nullableColor @ppxDecoder(module: "Color")
        nullableString @ppxCustomOpt(module: "NullableString")
    }
  }
`)
