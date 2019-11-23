module MyQuery: {
  type t = {
    .
    "mutationWithError": {
      .
      "value": option({. "stringField": string}),
      "errors":
        option(
          array({
            .
            "field": [ | `FIRST | `SECOND | `THIRD],
            "message": string,
          }),
        ),
    },
  };

  let make:
    unit =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    Js.t({.}) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let query: string;
  let makeVariables: unit => Js.Json.t;
};
