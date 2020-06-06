---
title: Future Added Values
---

`graphql-ppx` will add the polymorphic variant `\`FutureAddedValue(value)` by
default to both enum fields & union variants. This is in accordance to the
graphql specification, in order to build robust clients against potentially
changing server schemas.

[Lee Byron](https://github.com/leebyron), the co-creator of graphql, says the
[following](https://github.com/facebook/relay/issues/2351#issuecomment-368958022)
about this topic:

> These are generated as a reminder that GraphQL services often expand in
> capabilities and may return new enum values. To be future-proof, clients
> should account for this possibility and do something reasonable to avoid a
> broken product.

Adding this variant is intentional default behaviour of the ppx, to avoid
unintentional production bugs. You have however the option, to specifically
opt-out of this behaviour and disable the generation of this additional variant.
This could be useful, if you have absolute control over both the client and the
server schema and are confident, that they may never be out of sync.

To opt-out, you can specify the option `future_added_value: false`, either in
your `bsconfig.json` (see [config](https://beta.graphql-ppx.com/docs/config)),
or directly on your query.

Example:

```reason
module ByConfig = [%graphql
  {|
    {
      someQuery {
        enumField
      }
    }
|};
  {future_added_value: false}
];
```

The second way is to use the directive `@ppxOmitFutureValue` directly on your
queried field.

```reason
module ByDirective = [%graphql
  {|
    {
      someQuery {
        enumField @ppxOmitFutureValue
      }
    }
|}
];
```

```reason
// t_someQuery_enumField without config / directive
type t_someQuery_enumField = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ];
// t_someQuery_enumField with config / directive
type t_someQuery_enumField = [
    | `FIRST
    | `SECOND
    | `THIRD
  ];
```

**Please note:** Decoding the raw query result while having the future value
variant disabled, can lead to a `Not_found` exception being thrown if an
unexpected result is received.
