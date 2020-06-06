---
title: Operation
---

An GraphQL operation is a query, mutation or subscription. When you create an
operation using the GraphQL extension point, it creates a module. This module
has the following module type:

```reason
module type Operation = {
  include Definition;
  module Raw: {
    include Definition.Raw;
    type t_variables;
  };
  type t_variables;
  let makeVariables: (~exampleVariable: string, unit) => Raw.t_variables;
  let serializeVariables: t_variables => Raw.t_variables;
}
```

It includes the [`Definition`](definition) module type so everything in
`Definition` is also part of the `Operation` module type.

## Types

### Type `t_variables`

This is the the type of the variables in ReasonML data types.

#### Data Types

##### Nullable

Nullable variable values are represented as `option`. When this types is
serialized `option`'s are converted to `undefined`. Because `null` is also a
valid value, it is not representable in this data type. To set a value
explicitly `null` you can use `Raw.t_variables`.

##### Input Objects

Input objects are represented as records. The type is named as
`t_variables_InputObjectName`.

##### Other data types

Other data types are consistent with the types of `t`.

### Type `Raw.t_variables`

This is the (no cost) type of the variables. It's a record if there are
variables and it's `()` (unit), if there are no variables.

#### Data Types

##### Nullable

Nullable variable values are represented as `Js.Nullable.t`. It is important to
note that there is a difference between `Js.Nullable.null` and
`Js.Nullable.undefined`. `null` will be an explicit null, and `undefined` will
act as a missing field. Both are different things in the GraphQL API.

##### Input Objects

Input objects are represented as records. The type is named as
`Raw.t_variables_InputObjectName`.

##### Other data types

Other data types are consistent with the types of `Raw.t`.

## Bindings

### `makeVariables`

This is the creator function for `Raw.t_variables`. Often when you pass
variables to a library, that library expects the `Raw.t_variables` type. You can
make this using this function.

```reason
let variables = Query.makeVariables(~exampleVariable="something", ());
let result = Query.use(~variables, ())
```

### `serializeVariables`

If you rather create the variables yourself, you can construct the `t_variables`
record. Because most libraries expect `Raw.t_variables`, you can serialize the
record using this function.
