# graphql_ppx_re

> Experiments with [graphql_ppx](https://github.com/mhallin/graphql_ppx) in Reason

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

By default graphql_ppx uses `graphlq_schema.json` filed from your root directory. You can override it by provind env variable ovverriding it
