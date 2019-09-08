# graphql_ppx_re

[![npm version](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re.svg)](https://badge.fury.io/js/%40baransu%2Fgraphql_ppx_re)
[![Build Status](https://travis-ci.com/baransu/graphql_ppx_re.svg?branch=master)](https://travis-ci.com/baransu/graphql_ppx_re)

This project builds upon [graphql_ppx](https://github.com/mhallin/graphql_ppx). At this points it's basiclly original OCaml code reformated to Reason with small adjustments to build process.

### [Native Reason and BuckleScript 6.x support is coming!](https://github.com/baransu/graphql_ppx_re/pull/13)

## Developing

```
npm install -g esy@latest
esy install
esy build
```

## Running tests

```
cd tests
yarn
yarn test
```

## Configuration

You have to provide enviroment variable to configure certain features of graphq_ppx

#### GRAPHQL_PPX_APOLLO_MODE

Tells graphql_ppx to add \_\_typename to every object in a query. Usefull in case of using Apollo client to improve caching

#### GRAPHQL_PPX_SCHEMA

By default graphql_ppx uses `graphql_schema.json` filed from your root directory. You can override it by provind env variable overriding it
