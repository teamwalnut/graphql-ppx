---
title: Data Structures
---

GraphQL ppx converts GraphQL data and arguments from "pure" JSON data to
idiomatic ReasonML data structures:

- GraphQL objects are converted into records
- Unions, interfaces and enums are converted into
  [polymorphic variants](https://2ality.com/2018/01/polymorphic-variants-reasonml.html)
  with exhaustiveness checking
- Floats, ints, strings, booleans, id are converted into their corresponding
  native Reason/OCaml types
- Custom scalars are `Js.Json.t` and can be parsed using the `@ppxDecoder`
  directive
