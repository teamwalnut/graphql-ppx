module MyQuery: {
  type t = {. "listsInput": string};

  let make:
    (
      ~arg: {
              ..
              "nullableOfNullable": option(array(option(string))),
              "nullableOfNonNullable": option(array(string)),
              "nonNullableOfNullable": array(option(string)),
              "nonNullableOfNonNullable": array(string),
            },
      unit
    ) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let makeWithVariables:
    {
      .
      "arg": {
        .
        "nullableOfNullable": option(array(option(string))),
        "nullableOfNonNullable": option(array(string)),
        "nonNullableOfNullable": array(option(string)),
        "nonNullableOfNonNullable": array(string),
      },
    } =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let makeVariables:
    (
      ~arg: {
              ..
              "nullableOfNullable": option(array(option(string))),
              "nullableOfNonNullable": option(array(string)),
              "nonNullableOfNullable": array(option(string)),
              "nonNullableOfNonNullable": array(string),
            },
      unit
    ) =>
    Js.Json.t;

  let query: string;
};
