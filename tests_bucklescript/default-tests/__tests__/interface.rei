module QueryWithFragments: {
  type t = {
    .
    "users":
      array(
        [
          | `User(Js.String.t, {. "id": string})
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
};

module QueryWithoutFragments: {
  type t = {. "users": array([ | `User(Js.String.t, {. "id": string})])};

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
};
