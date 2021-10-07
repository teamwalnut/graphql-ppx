<p align="center">
    <img width="200" src="https://beta.graphql-ppx.com/img/logo.svg" alt="Logo">
  	<br><br>
    Typesafe GraphQL operations and fragments in ReasonML
</p>

<p align="center">
  <a href="https://github.com/reasonml-community/graphql-ppx/actions">
    <img src="https://github.com/reasonml-community/graphql-ppx/workflows/graphql-ppx-pipeline/badge.svg" alt="Build Status" />
  </a>
  <a href="https://badge.fury.io/js/%40reasonml-community%2Fgraphql-ppx.svg">
    <img src="https://badge.fury.io/js/%40reasonml-community%2Fgraphql-ppx.svg" alt="npm version" />
  </a>
</p>

<p align="center">
  <a href="#documentation">Documentation</a> •
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#usage">Usage</a> •
  <a href="#roadmap">Roadmap</a> •
  <a href="#contributing">Contributing</a> •
  <a href="#license">License</a> •
  <a href="#acknowledgements">Acknowledgements</a>
</p>

## Documentation

[Go to the official documentation](https://beta.graphql-ppx.com)

## Features

- Language level GraphQL primitives

- Building block for GraphQL clients

- 100% type safe

## Installation

### Schema

`graphql-ppx` needs your graphql schema to be available in the form of a
`graphql_schema.json` file.

The easiest way to add this to your project is using an
[introspection query](https://github.com/graphql/graphql-js/blob/master/src/utilities/introspectionQuery.js)
to your backend. You can do this using `get-graphql-schema`:

```sh
npx get-graphql-schema ENDPOINT_URL -j > graphql_schema.json
```

With `ENDPOINT_URL` being the URL of your GraphQL endpoint.

### Cache

`graphql-ppx` will generate a `.graphql_ppx_cache` folder alongside your JSON
schema to optimize parsing performance. If you're using a version control
system, you don't need to check it in.

The next pages will provide further installation instructions whether you are
using `graphql-ppx` with Bucklescript or using Reason Native.

### Bucklescript

First, add it to you dependencies using `npm` or `yarn`:

```sh
yarn add @reasonml-community/graphql-ppx --dev
# or
npm install @reasonml-community/graphql-ppx  --saveDev
```

Second, add it to `ppx-flags` and `bs-dependencies` in your `bsconfig.json`:

```json
"ppx-flags": ["@reasonml-community/graphql-ppx/ppx"],
"bs-dependencies": ["@reasonml-community/graphql-ppx"]
```

### Native

#### Caution!

The Bucklescript version of `graphql-ppx` was almost completely rewritten for the
1.0 release, with many improvements and changes. This documentation will focus
on the API of the bucklescript version. This means that most of the examples
won't apply for the Reason Native version. Please take a look at the
[old documentation](https://github.com/reasonml-community/graphql-ppx/tree/v0.7.1).
At the same time we welcome contributions to modernize the Reason Native version
of `graphql-ppx`
:::

You need to provide the following dependency in your `esy.json` file

```json
{
  "dependencies": {
    "graphql-ppx": "*"
  },
  "resolutions": {
    "graphql-ppx": "reasonml-community/graphql-ppx:esy.json#<use latest stable commit from master>"
  }
}
```

and update your `dune` file:

```
(preprocess (pps graphql_ppx))
```

## Usage

Make your first query:

### Reason

```reason
[%graphql {|
  query UserQuery {
    user {
      id
      name
    }
  }
|}];
```

### ReScript

```rescript
%graphql(`
  query UserQuery {
    user {
      id
      name
    }
  }
`)
```

[Open getting started in the docs](https://beta.graphql-ppx.com/docs/getting-started)

## Roadmap

See our [development board](https://github.com/reasonml-community/graphql-ppx/projects/1) for a list of selected features and issues.

## Contributing

We'd love your help improving `graphql-ppx`!

Take a look at our [Contributing Guide](https://beta.graphql-ppx.com/docs/contributing) to get started.

## License

Distributed under the MIT License. See [LICENSE](LICENSE) for more information.

## Acknowledgements

Thanks to everyone who [contributed](https://github.com/reasonml-community/graphql-ppx/graphs/contributors) to `graphql-ppx`!

This project builds upon [mhallin/graphql_ppx](https://github.com/mhallin/graphql_ppx). It wouldn't be possible without
great work of [mhallin/graphql_ppx contributors](https://github.com/mhallin/graphql_ppx/graphs/contributors).
