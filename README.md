# graphql_ppx

[![npm version](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re.svg)](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re)

> Reason/OCaml PPX (PreProcessor eXtension) helping with creating type-safe, compile time validated GraphQL queries
> generating response decoders.

This project builds upon [mhallin/graphql_ppx](https://github.com/mhallin/graphql_ppx). It wouldn't be possible without
great work of [mhallin/graphql_ppx contributors](https://github.com/mhallin/graphql_ppx/graphs/contributors).

## Installation

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

## Native

If you want to use native version edit your `esy.json` file

```json
{
  "dependencies": {
    "graphql_ppx": "*"
  },
  "resolutions": {
    "graphql_ppx": "reasonml-community/graphql_ppx:esy.json#<use latest stable commit from master>"
  }
}
```

and update your `dune` file:

```
(preprocess (pps graphql_ppx))
```

## Usage

This plugin requires a `graphql_schema.json` file to exist somewhere in the
project hierarchy, containing the result of sending an [introspection
query](https://github.com/graphql/graphql-js/blob/master/src/utilities/introspectionQuery.js)
to your backend. The easiest way to do this is by using `get-graphql-schema`:

```sh
npx get-graphql-schema ENDPOINT_URL -j > graphql_schema.json
```

## Ignore `.graphql_ppx_cache` in your version control

`graphql_ppx` will generate a `.graphql_ppx_cache` folder alongside your JSON
schema to optimize parsing performance. If you're
using a version control system, you don't need to check it in.

## Features

- Objects are converted into records
- Enums are converted into [polymorphic
  variants](https://2ality.com/2018/01/polymorphic-variants-reasonml.html)
- Floats, ints, strings, booleans, id are converted into their corresponding native
  Reason/OCaml types.
- Custom scalars are parsed as `Js.Json.t`, and can be parsed using the `@ppxDecoder` directive
- Arguments with input objects
- Using `@skip` and `@include` will force non-optional fields to become
  optional.
- Unions are converted to polymorphic variants, with exhaustiveness checking.
  This only works for object types, not for unions containing interfaces.
- Interfaces are also converted into polymorphic variants. Overlapping interface
  selections and other more uncommon use cases are not yet supported.
- Support for fragments
- Required arguments validation

## Typical use

GraphQL PPX is a utility to work with the GraphQL protocol in ReasonML.
Typically this PPX is being used in combination with a GraphQL client. Popular
clients include [Reason Apollo Hooks](https://github.com/Astrocoders/reason-apollo-hooks/commits/master)
or [Reason URQL](https://github.com/FormidableLabs/reason-urql). They also
provide a more end-to-end getting started. This documentation will focus on how
to create queries and fragments, and parse responses.

## Defining a Query

You can define a query in your ReasonML file with the following code

```reason
[%graphql {|
  query UserQuery {
    user {
      id
      role
    }
  }
|}];
```

This will create the `UserQuery` module. This module has the following
contents assigned:

### Let bindings

#### Basic

- `query` (`string`), the GraphQL query or mutation
- `parse` (`UserQuery.t_raw => UserQuery.t`), the function to parse the raw
  GraphQL response into ReasonML types.
- `makeVariables` (`(~your, ~arguments, ()) => Js.Json.t`): a
  function that takes labeled arguments to produce the variables that can be
  sent together with the query. This will also validate and type-check the
  variables.
- `definition`: the module contents packaged. This is usually what you provide
  to the client for ergonomics so you don't have to pass multiple arguments per
  query

#### Advanced

- `serialize` (`t => t_raw`): this is the opposite of parse.
  Sometimes you need to convert the ReasonML representation of the response back
  into the raw JSON representation. Usually this is used within the GraphQL
  client for things like updating the internal cache.
- `serializeVariables` (`t_variables => Js.Json.t`): Convert the
  variables (a record) to a Js.Json.t representation as an alternative to the
  labeled function
- `makeInputObject{YourInputObject}` - a labeled function to create
  `YourInputObject`: This is helpful when you have an input object with many
  optional values (works exactly the same as makeVariables)
- `fromJSON` (`Js.Json.t => t_raw`): With this function you can
  convert a Js.Json.t response to a `t_raw` response. It is a no-op and just
  casts the type.

### Types

- `t`: the parsed response of the query
- `t_raw`: the unparsed response. This is basically the exact shape of the raw
  response before it is parsed into more ergonomic ReasonML types like `option`
  instead of `Js.Json.t`, variants etc.
- `t_variables`: the variables of the query or mutation

GraphQL objects, variables and input objects are typed as records for `t`,
`t_raw` and `t_variables`. The types are named according to the hierarchy. Each
step in the hierarchy is split using an underscore. So the type of the user
object in the query above is `t_user` if there would be a field that contained
friends of the user it would be called `t_user_friends`.

## Alternative ways of using `%graphql`

When using GraphQL like this:

```reason
module UserQuery = [%graphql {|
  query UserQuery {
    user {
      id
      role
    }
  }
|}];
```

It will have the same effect as the result above. However you can now rename the
query module.

You can also do this:

```reason
module UserQueries = {
  [%graphql {|
    query UserQuery {
      user {
        id
        role
      }
    }
  |}];
};
```

This will create a parent module (the query now will be:
`UserQueries.UserQuery`)

You can define multiple operations or fragments within a single GraphQL extension
point.

If you do not want to put the query contents in a module, but to be in effect
"opened" in the current module you can use the `inline` option:

```reason
[%graphql {|
  query UserQuery {
    user {
      id
      role
    }
  }
|};
{inline: true}
];
```

## Reuse

Records in Reason are nominally typed. Even if a records contains exactly the
same fields as another record, it will be seen as a different type, and they are
not compatible. That means that if you want to create an `createAvatar` function
for a `User`, you'd be able to accept for instance `UserQuery.t_user` as an
argument. That's all great, but what if you have another query where you also
would like to create an avatar. In most cases Fragments are the solution here.

### Fragments

With fragments you can define reusable pieces that can be shared between
queries. You can define a fragment in the following way

```reason
[%graphql {|
  fragment Avatar_User on User {
    id
    name
    smallAvatar: avatar(pixelRatio: 2, width: 60, height: 60) {
      url
    }
  }

  query UserQuery {
    user {
      id
      role
      ...Avatar_User
    }
  }
|}]
```

This generates the module `Avatar_User` as the fragment. The `createAvatar`
can now accept `Avatar_User.t` which include all the fields of the fragment.

How to we get this from the query? When you use the spread operator with the
module name, an extra field is created on the `t_user` record with the name
`avatar_User` (same as the fragment module name but with a lowercase first
letter). This is the value that has the type `Avatar_User.t` containing all the
necessary fields.

If you want to change the default name of the fragment you
can use a GraphQL alias (`avatarFragment: ...AvatarUser`).

When there is just the fragment spread and no other fields on an object, there
is no special field for the fragment necessary. So if this is the query:

```reason
[%graphql {|
  query UserQuery {
    user {
      ...Avatar_User
    }
  }
|}]
```

Then `user` will be of the type `Avatar_User.t`.

#### Variables within fragments

Sometimes fragments need to accept variables. Take our previous fragment. If we
would like to pass the pixelRatio as a variable as it might vary per device. We
can do this as follows:

```reason
[%graphql {|
  fragment Avatar_User on User @argumentDefinitions(pixelRatio: {type: "Float!"}) {
    id
    name
    smallAvatar: avatar(pixelRatio: 2, width: 60, height: 60) {
      url
    }
  }

  query UserQuery($pixelRatio: Float!) {
    user {
      id
      role
      ...Avatar_User @arguments(pixelRatio: $pixelRatio)
    }
  }
|}]
```

To be able to typecheck these variables and make sure that the types are correct,
there are no unused variables or variables that are not defined, we introduce
two directives here `argumentDefinitions` and `arguments`, these are taken from
[Relay](https://relay.dev/docs/en/fragment-container#argumentdefinitions). But
they have nothing to do with the relay client (we just re-use this convention).

Note that you cannot rename variables in the `@arguments` directive so the name
of the variable and the name of the key must be the same. This is because
GraphQL PPX does not manipulate variable names and just makes use of the fact
that fragments can use variables declared in the query.

There is a compile error raised if you define variables that are unused. If you
(temporarily) want to define unused variables you can prepend the variable name
with an underscore.

#### `bsAs`

An ecape hatch for when you don't want GraphQL PPX to create a record type, you
can supply one yourself. This also makes reusability possible. We recommend
fragments however in most cases as they are easier to work, are safer and don't
require defining separate types.

```reason
type t_user = {
  id: string
  role: string
}

[%graphql {|
  query UserQuery {
    user @bsAs(type: "t_user") {
      id
      role
    }
  }
|}]
```

### Custom field decoders

If you've got a custom scalar, or just want to convert e.g. an integer to a
string to properly fit a record type (see above), you can use the `@ppxDecoder`
directive to insert a custom function in the decoder:

```reason
module StringHeight = {
  let parse = (height) => string_of_float(height);
  let serialize = (height) => float_of_string(height);
  type t = string;
}


module HeroQuery = [%graphql {|
{
  hero {
    name
    height @ppxDecoder(module: "StringHeight")
    mass
  }
}
|}];
```

In this example, `height` will be converted from a float to a string in the
result. Using the `module` argument, you can specify any decoder module with
the functions `parse`, `serialize` and type `t`.

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
  Api.sendQuery(
    ~variables=SignUpQuery.makeVariables(
      ~name="My name",
      ~email="email@example.com",
      ~password="secret",
      (),
    ),
    SignUpQuery.definition
  )
  |> Promise.then_(response =>
       (
         switch (response.signUp) {
         | `User(user) => Js.log2("Signed up a user with name ", user.name)
         | `Errors(errors) => Js.log2("Errors when signing up: ", errors)
         }
       )
       |> Promise.resolve
     );

```

This helps with the fairly common pattern for mutations that can fail with
user-readable errors.

## Troubleshooting

### "Type ... doesn't have any fields"

Sometimes when working with union types you'll get the following error.

```
Fatal error: exception Graphql_ppx_base__Schema.Invalid_type("Type IssueTimelineItems doesn't have any fields")
```

This is an example of a query that will result in such error:

```graphql
nodes {
  __typename
  ... on ClosedEvent {
    closer {
      __typename
      ... on PullRequest {
        id
        milestone { id }
      }
    }
  }
}
```

This is because we allow querying union fields only in certain cases. GraphQL provides the `__typename` field but it's not present in GraphQL introspection query thus `graphql_ppx` doesn't know that this field exists.
To fix your query simply remove `__typename`. It's added behinds a scene as an implementation detail and serves us as a way to decide which case to select when parsing your query result.

This is an example of a correct query:

```graphql
nodes {
  ... on ClosedEvent {
    closer {
      ... on PullRequest {
        id
        milestone { id }
      }
    }
  }
}
```

## Configuration

If you need to customize certain features of `graphql_ppx` you can provide ppx arguments to do so:

### -apollo-mode

By default `graphql_ppx` adds `__typename` only to fields on which we need those informations (Unions and Interfaces). If you want to add `__typename` on every object in a query you can specify it by using `-apollo-mode` in `ppx-flags`. It's usefull in case of using `apollo-client` because of it's cache.

```json
"ppx-flags": [
  ["@baransu/graphql_ppx_re/ppx", "-apollo-mode",]
],
```

### -schema

By default `graphql_ppx` uses `graphql_schema.json` file from your root directory. You can override it by providing `-schema` argument in `ppx-flags` to overriding it.

```json
"ppx-flags": [
  ["@baransu/graphql_ppx_re/ppx", "-schema ../graphql_schema.json"]
],
```

# Query specific configuration

If you want to use multiple schemas in your project it can be provided as a secondary config argument in your graphql ppx definition.

```reason
module MyQuery = [%graphql
  {|
    query pokemon($id: String, $name: String) {
      pokemon(name: $name, id: $id) {
        id
        name
      }
    }
  |};
  {schema: "pokedex_schema.json"}
];
```

This will use the `pokedex_schema.json` instead of using the default `graphql_schema.json` file.

This opens up the possibility to use multiple different GraphQL APIs in the same project.

**Note** the path to your file is based on where you run `bsb`. In this case `pokedex_schema.json` is a sibling to `node_modules`.

# Supported platforms

`graphql_ppx` somes with prebuild binaries for `linux-x64`, `darwin-x64` and `win-x64`. If you need support for other platform, please open an issue.

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
cd tests_bucklescript
npm test
```

### Native

For native run:

```
esy dune runtest -f
```
