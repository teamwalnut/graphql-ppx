---
title: Configuration
---

## Configuration inside of graphql definitions

If you want to use multiple schemas in your project it can be provided as a
secondary config argument in your `graphql-ppx` definition.

```reason
module MyQuery = [%graphql
  {|
    query pokemon($id: String, $name: String)
      @ppxConfig(schema: "pokedex_schema.json") {
      pokemon(name: $name, id: $id) {
        id
        name
      }
    }
  |}
];
```

## Configuration in `bsconfig.json`

You can configure `graphql-ppx` globally using `bsconfig.json` under the
`graphql` key.

```json
{
  "name": "my-project",
  "graphql": {
    "schema": "pokedex_schema.json"
  }
}
```

## Command line configuration

You can also configure `graphql-ppx` using command line arguments of the `ppx`
executable.

for instance:

```json
"ppx-flags": [
  ["@reasonml-community/graphql-ppx/ppx", "-schema ../graphql_schema.json"]
],
```

or

```json
"ppx-flags": [
  ["@reasonml-community/graphql-ppx/ppx", "-apollo-mode",]
],
```

## Configuration options

The configuration options are in `kebab-case` for command line arguments and
`bsconfig.json` configuration, but in `camelCase` for directives on a GraphQL
definition.

### schema

By default `graphql-ppx` uses `graphql_schema.json` file from your root
directory. You can override it with the `schema` option.

This opens up the possibility to use multiple different GraphQL APIs in the same
project.

**Note** the path to your file is based on where you run `bsb`. In this case
`pokedex_schema.json` is a sibling to `node_modules`.

### objects

Globally compile definitions to use objects instead of records (to have a
migration path with with 0.x versions of `graphql-ppx`.

### fragmentInQuery

This option can be either "include" or "exclude". It configures if the fragment
definition is included in the query. Necessary for Apollo, but not necessary for
for instance Gatsby.

### apolloMode

By default `graphql-ppx` adds `__typename` only to fields on which we need those
informations (Unions and Interfaces). If you want to add `__typename` on every
object in a query you can specify it by using `apolloMode`.

### customFields

Custom fields allow a mapping from GraphQL type names to modules with a type
(`t`), `parse` and `serialize` functions.

### template tag literal support

These options facilitate template tag literal support

- `templateTag`

- `templateTagLocation`

- `templateTagLocation`

- `templateTagImport`

- `templateTagReturnType`

### extension

We allow the extension of the GraphQL definition modules using the following
configuration:

- `extendQuery`

- `extendQueryNoRequiredVariables`

- `extendMutation`

- `extendMutationNoRequiredVariables`

- `extendSubscription`

- `extendSubscriptionNoRequiredVariables`

- `extendFragment`
