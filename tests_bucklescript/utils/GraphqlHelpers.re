module Color = {
  type t =
    | Red
    | Green
    | Blue;
  let parse = json =>
    switch (json |> Js.Json.decodeString) {
    | Some("green") => Green
    | Some("blue") => Blue
    | Some("red")
    | Some(_)
    | None => Red
    };
  let serialize = color =>
    (
      switch (color) {
      | Red => "red"
      | Green => "green"
      | Blue => "blue"
      }
    )
    |> Js.Json.string;
};
module DateTime = {
  type t = Js.Date.t;
  let parse = json =>
    switch (json |> Js.Json.decodeString) {
    | Some(str) => str |> Js.Date.fromString
    | None => Js.Date.make()
    };
  let serialize = date => date |> Js.Date.toISOString |> Js.Json.string;
};
