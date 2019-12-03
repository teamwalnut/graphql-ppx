module QueryWithFragments: {
  type t = {
    .
    "users":
      array(
        [
          | `User({. "id": string})
          | `AdminUser(
              {
                .
                "id": string,
                "name": string,
              },
            )
          | `AnonymousUser(
              {
                .
                "id": string,
                "anonymousId": int,
              },
            )
        ],
      ),
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

module QueryWithoutFragments: {
  type t = {. "users": array([ | `User({. "id": string})])};

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
