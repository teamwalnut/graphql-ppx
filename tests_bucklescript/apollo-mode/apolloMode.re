open Jest;
open Expect;

module BasciQuery = [%graphql
  {|
  {
    first: nestedObject {
      inner {
        inner {
          field
        }
      }
    }

    second: nestedObject {
      inner {
        inner {
          f1: field
          f2: field
        }
      }
    }
  }
|}
];

module Subscription = [%graphql
  {|
  subscription {
    simpleSubscription {
      ...on Dog {
        name
      }
      ...on Human {
        name
      }
    }
  }
|}
];

describe("Apollo mode", () => {
  let typenameRegex = [%bs.re {|/__typename/g|}];

  test("Adds __typename to objects", () =>
    BasciQuery.query
    |> Js.String.match(typenameRegex)
    |> Belt.Option.map(_, Array.length)
    |> expect
    |> toEqual(Some(7))
  );

  test("subscription don't have top level __typename", () =>
    Subscription.query
    |> Js.String.match(typenameRegex)
    |> Belt.Option.map(_, Array.length)
    |> expect
    /* 3 because on each union case and in simpleSubscription */
    |> toEqual(Some(3))
  );
});
