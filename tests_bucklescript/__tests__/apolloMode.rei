module BasciQuery: {
  let query: string;
  type t = {
    .
    "first": {. "inner": option({. "inner": option({. "field": string})})},
    "second": {
      .
      "inner":
        option({
          .
          "inner":
            option({
              .
              "f1": string,
              "f2": string,
            }),
        }),
    },
  };

  let parse: Js.Json.t => t;
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
  let makeVariables: unit => Js.Json.t;
};
module Subscription: {
  let query: string;
  type t = {
    .
    "simpleSubscription": [
      | `Dog({. "name": string})
      | `Human({. "name": string})
    ],
  };

  let parse: Js.Json.t => t;
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
  let makeVariables: unit => Js.Json.t;
};
