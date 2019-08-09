# graphql_ppx_re

This project builds upon [graphql_ppx](https://github.com/mhallin/graphql_ppx). At this points it's basiclly original OCaml code reformated to Reason with small adjustments to build process.

Right now I don't focus on Native Reason. If you want to help, PRs welcome.

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
