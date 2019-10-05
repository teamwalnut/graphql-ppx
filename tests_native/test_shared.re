let yojson: module Alcotest.TESTABLE with type t = Yojson.Basic.t =
  (module
   {
     type t = Yojson.Basic.t;

     let pp = (formatter, t) =>
       Format.pp_print_text(formatter, Yojson.Basic.pretty_to_string(t));

     let equal = (==);
   });

let test_json = (a, b) => Alcotest.check(yojson, "JSON equality", a, b);

let print_option = (inner, formatter) =>
  fun
  | None => Format.pp_print_string(formatter, "None")
  | Some(v) => Format.fprintf(formatter, "Some(@[%a@])", inner, v);

let print_array = (inner, formatter, value) => {
  open Format;
  pp_print_string(formatter, "[ ");
  Array.iteri(
    (idx, v) => {
      if (idx > 0) {
        pp_print_string(formatter, "; ");
      };
      pp_open_hovbox(formatter, 1);
      inner(formatter, v);
      pp_close_box(formatter, ());
    },
    value,
  );
  pp_print_string(formatter, " ]");
};

let array_zipmap = (f, a, b) => {
  let min = min(Array.length(a), Array.length(b));
  Array.init(min, i => f(a[i], b[i]));
};

let opt_eq = (f, a, b) =>
  switch (a, b) {
  | (Some(a), Some(b)) => f(a, b)
  | (None, None) => true
  | _ => false
  };
