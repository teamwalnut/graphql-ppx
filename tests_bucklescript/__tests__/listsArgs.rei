module MyQuery: {
  type t = {. "listsInput": string};

  let make:
    (
      ~nullableOfNullable: array(option(string))=?,
      ~nullableOfNonNullable: array(string)=?,
      ~nonNullableOfNullable: array(option(string)),
      ~nonNullableOfNonNullable: array(string),
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
      "nullableOfNullable": option(array(option(string))),
      "nullableOfNonNullable": option(array(string)),
      "nonNullableOfNullable": array(option(string)),
      "nonNullableOfNonNullable": array(string),
    } =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let query: string;
  let makeVariables:
    (
      ~nullableOfNullable: array(option(string))=?,
      ~nullableOfNonNullable: array(string)=?,
      ~nonNullableOfNullable: array(option(string)),
      ~nonNullableOfNonNullable: array(string),
      unit
    ) =>
    Js.Json.t;
};
