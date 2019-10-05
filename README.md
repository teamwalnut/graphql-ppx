# graphql_ppx_re

[![npm version](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re.svg)](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re)

### [Native Reason and BuckleScript 6.x support is coming!](https://github.com/baransu/graphql_ppx_re/pull/13)

> Reason/OCaml PPX (PreProcessor eXtension) helping with creating type-safe, compile time validated GraphQL queries generating response decoders.

This project builds upon [mhallin/graphql_ppx](https://github.com/mhallin/graphql_ppx). It wouldn't be possible without great work of [mhallin/graphql_ppx contributors](https://github.com/mhallin/graphql_ppx/graphs/contributors).

# Installation

First, add it to you dependencies using `npm` or `yarn`:

```sh
yarn add @baransu/graphql_ppx_re --dev
# or
npm install @baransu/graphql_ppx_re  --saveDev
```

Second, add it to `ppx-flags` in your `bsconfig.json`:

```json
"ppx-flags": ["@baransu/graphql_ppx_re/ppx"]
```

# Usage

This plugin requires a `graphql_schema.json` file to exist somewhere in the
project hierarchy, containing the result of sending an [introspection
query](https://github.com/graphql/graphql-js/blob/master/src/utilities/introspectionQuery.js)
to your backend. One of tools helping with it is [graphql-cli](https://www.npmjs.com/package/graphql-cli). When you already have `schema.graphql` file you have to convert it to `*.json` file. You can use [gql-tools](https://www.npmjs.com/package/gql-tools) to convert your `schema.graphql` to `graphql_schema.json` file. It's convenient to keep those two steps separated in `package.json`. It will help you when you have to change your local schema without sync with your backend.

```json
{
  "scripts": {
    "sync-schema": "graphql get-schema && node ./generateFragmentTypes.js",
    "generate-ppx-schema": "node ./node_modules/gql-tools/cli/gqlschema.js -o graphql_schema.json schema.graphql"
  }
}
```

## Ignore `.graphql_ppx_cache` in your version control

`graphql_ppx_re` will generate a `.graphql_ppx_cache` folder alongside your JSON
schema to optimize parsing performance. If you're
using a version control system, you don't need to check it in.

# Limitations

While `graphql_ppx_re` covers a large portion of the GraphQL spec, there are still
some unsupported areas:

- Not all GraphQL validations are implemented. It will _not_ validate argument
  types and do other sanity-checking of the queries. The fact that a query
  compiles does not mean that it will pass server-side validation.
- Fragment support is limited and not 100% safe - because `graphql_ppx_re` only can
  perform local reasoning on queries, you can construct queries with fragments
  that are invalid.

# Features

- Objects are converted into `Js.t` objects
- Enums are converted into [polymorphic
  variants](https://2ality.com/2018/01/polymorphic-variants-reasonml.html)
- Floats, ints, strings, booleans, id are converted into their corresponding native
  Reason/OCaml types.
- Custom scalars are parsed as `Js.Json.t`
- Arguments with input objects
- Using `@skip` and `@include` will force non-optional fields to become
  optional.
- Unions are converted to polymorphic variants, with exhaustiveness checking.
  This only works for object types, not for unions containing interfaces.
- Interfaces are also converted into polymorphic variants. Overlapping interface
  selections and other more uncommon use cases are not yet supported.
- Basic fragment support

## Extra features

By using some directives prefixed `bs`, `graphql_ppx_re` lets you modify how the
result of a query is parsed. All these directives will be removed from the query
at compile time, so your server doesn't have to support them.

### Record conversion

While `Js.t` objects often have their advantages, they also come with some
limitations. For example, you can't create new objects using the spread (`...`)
syntax or pattern match on their contents. Since they are not named, they also
result in quite large type error messages when there are mismatches.

Reason/OCaml records, on the other hand, can be pattern matched, created using the
spread syntax, and give nicer error messages when they mismatch. `graphql_ppx_re`
gives you the option to decode a field as a record using the `@bsRecord`
directive:

```reason
type hero = {
  name: string,
  height: number,
  mass: number
};

module HeroQuery = [%graphql {|
{
  hero @bsRecord {
    name
    height
    mass
  }
}
|}];
```

Note that the record has to already exist and be in scope for this to work.
`graphql_ppx_re` will not _create_ the record. Even though this involves some
duplication of both names and types, type errors will be generated if there are
any mismatches.

### Custom field decoders

If you've got a custom scalar, or just want to convert e.g. an integer to a
string to properly fit a record type (see above), you can use the `@bsDecoder`
directive to insert a custom function in the decoder:

```reason
module HeroQuery = [%graphql {|
{
  hero {
    name
    height @bsDecoder(fn: "string_of_float")
    mass
  }
}
|}];
```

In this example, `height` will be converted from a float to a string in the
result. Using the `fn` argument, you can specify any function literal you want.

### Non-union variant conversion

If you've got an object which in practice behaves like a variant - like `signUp`
above, where you _either_ get a user _or_ a list of errors - you can add a
`@bsVariant` directive to the field to turn it into a polymorphic variant:

```reason
module SignUpQuery = [%graphql
  {|
mutation($name: String!, $email: String!, $password: String!) {
  signUp(email: $email, email: $email, password: $password) @bsVariant {
    user {
      name
    }

    errors {
      field
      message
    }
  }
}
|}
];

let _ =
  SignUpQuery.make(
    ~name="My name",
    ~email="email@example.com",
    ~password="secret",
    (),
  )
  |> Api.sendQuery
  |> Promise.then_(response =>
       (
         switch (response##signUp) {
         | `User(user) => Js.log2("Signed up a user with name ", user##name)
         | `Errors(errors) => Js.log2("Errors when signing up: ", errors)
         }
       )
       |> Promise.resolve
     );

```

This helps with the fairly common pattern for mutations that can fail with
user-readable errors.

### Alternative `Query.make` syntax

When you define a query with variables, the `make` function will take
corresponding labelled arguments. This is convenient when constructing and
sending the queries yourself, but might be problematic when trying to abstract
over multiple queries.

For this reason, another function called `makeWithVariables` is _also_
generated. This function takes a single `Js.t` object containing all variables.

```reason
module MyQuery = [%graphql
  {|
  mutation ($username: String!, $password: String!) {
    ...
  }
|}
];

/* You can either use `make` with labelled arguments: */
let query = MyQuery.make(~username="testUser", password = "supersecret", ());

/* Or, you can use `makeWithVariables`: */
let query =
  MyQuery.makeWithVariables({
    "username": "testUser",
    "password": "supersecret",
  });
```

### Getting the type of the parsed value

If you want to get the type of the parsed and decoded value - useful in places
where you can't use Reason/OCaml's type inference - use the `t` type of the query
module:

```reason
module MyQuery = [%graphql {| { hero { name height }} |}];

/* This is something like Js.t({ . hero: Js.t({ name: string, weight: float }) }) */
type resultType = MyQuery.t;
```

# Configuration

If you need customize certain features of `graphql_ppx_re` you can provide environment variables do so:

### GRAPHQL_PPX_APOLLO_MODE

Tells graphql_ppx to add `__typename` to every object in a query. Usefull in case of using `apollo-client`.

### GRAPHQL_PPX_SCHEMA

By default graphql_ppx uses `graphql_schema.json` filed from your root directory. You can override it by providing env variable overriding it.

# Supported platforms

`graphql_ppx_re` somes with prebuild binaries for `linux-x64`, `darwin-x64` and `win-x64`. If you need support for other platform, please open an issue.

# Contributing

## Developing

```
npm install -g esy@latest
esy install
esy build
```

## Running tests

### BuckleScript

```
cd tests
yarn
yarn test
```

### Native

```
esy dune runtest -f
```
