---
title: Definition
---

A GraphQL definition (an [operation](operation) or a fragment) has the following
module type:

```reason
module type Definition = {
  module Raw: {
    type t;
  };

  type t;

  let query: string;

  let parse: Raw.t => t;
  let serialize: t => Raw.t;

  let unsafe_fromJson: Js.Json.t => Raw.t;
  let toJson: Raw.t => Js.Json.t;
}
```

## Types

### Type `t`

This is the parsed GraphQL data. This is the data type that you interact with in
most cases.

#### Data types

##### Objects

Objects are encoded as records

##### Nullable

Nullable are encoded as `option`

##### Unions

Unions are encoded as polymorphic variants. The members are encoded as
`` `Option1(t_myUnion_Option1) ``

##### Lists

Arrays are just typed as `array`.

##### Interfaces

Interfaces can be typed in three way. If you **justt** have inline fragments,
they are typed as polymorphic variants. If you just have shared fields, they are
just typed as a record. In case you have both, it's typed as a record with an
extra `fragment` field:

```reason
type t_user = {
  fragment: t_user_User
  id: string
  ...any other fields you might have
}
```

##### Scalars (String / Int / Float)

These map to their native types.

##### Fragment spreads

Fragment spreads are encoded as their fragment type when the spread is the only
field in an object. For instance:

```reason
[%graphql
  {|
  query MyQuery {
    myField {
      ...MyFragment
    }
  |}
]
```

The type of `myField` is `MyFragment.t`.

When we have more fields:

```reason
[%graphql
  {|
  query MyQuery {
    myField {
      ...MyFragment
      otherField
    }
  |}
]
```

The type will become a record with the keys `otherField` as the other field and
`myFragment` as `MyFragment.t`.

##### Custom scalars

Custom scalars are generally `Js.Json.t` unless you use
[custom fields](custom-fields)

### Type `Raw.t`

This is the (no cost) type of the JSON compatible GraphQL data. You can use this
directly, but usually you would use the parsed data. This is the raw data with
some conversions to ReasonML data types. This is a record.

#### Data types

##### Objects

Objects are encoded as records

##### Nullable

Nullable are encoded as `Js.Nullable.t`

##### Unions

Unions are encoded as an opaque type. Each union has a member type definition.

So if we have `t_myUnion` (opaque), for each member type we have another type
(`t_myUnion_Option1`). If you know what type a union is you can cast it into the
specific type with zero cost.

##### Lists

GraphQL lists are typed as `array`.

##### Interfaces

Interfaces are typed just like unions. We do not allow shared field at the
moment.

##### Scalars (String, Float, Int)

These map to their native types.

##### Fragment spreads

Fragment spreads are encoded as their fragment type when the spread is the only
field in an object. For instance:

```reason
[%graphql
  {|
  query MyQuery {
    myField {
      ...MyFragment
    }
  |}
]
```

The type of `myField` is `MyFragment.Raw.t`.

When we have more fields:

```reason
[%graphql
  {|
  query MyQuery {
    myField {
      ...MyFragment
      otherField
    }
  |}
]
```

The type becomes an opaque type that can be cast to `Js.Json.t` (or
`MyFragment.Raw.t`).

##### Custom scalars

Custom scalars are represented as `Js.Json.t`

## Bindings

### `query`

This is the plain GraphQL query string.

### `parse`

The plain JSON compatible data (`Raw.t`) is of limited use.

- Nullable values are represented as `Js.Nullable.t`
- Unions are opaque types because they can not be represented in Reason
- Enums are strings

It might be useful for simple data, and the upside is that it is 100% zero cost.

However to get the best developer experience we need to slightly transform the
JavaScript data to convert it to ReasonML types. Including:

- Converting `Js.Nullable.t` to option types
- Converting Unions to Polymorphic Variants
- Converting Enums to Polymorphic Variants

`parse` is a generated function that does this work for you. It tries to be the
lowest cost function in order to transform the data to these ReasonML types.

This function is usually being used internally within the GraphQL client.

### `serialize`

Sometimes you'd like to serialize the `t` data type back to the JSON compatible
`Raw.t`. For instance when you change the data and you'd like to updat the
cache. Or when you want to construct an optimistic update of a GraphQL response.

`serialize` offers this functionality.

This function is usually being used interally within the GraphQL client.

### `unsafe_fromJson`

This function is a zero cost function that converts a `Js.Json.t` to `Raw.t`. We
have to be sure that this is a valid server response from the specific
operation, or is the data of a fragment.

Because `Js.Json.t` is a generic type this function is unsafe. It will only work
properly for a specific shape of `Js.Json.t` (response of the query).

### `toJson`

This is a zero cost function that will convert `Raw.t` to `Js.Json.t`.
