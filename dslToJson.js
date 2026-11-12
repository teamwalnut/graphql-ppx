#!/usr/bin/env node
var fs = require("fs");
const { buildSchema, graphqlSync, getIntrospectionQuery } = require("graphql");
process.stdout.write(
  JSON.stringify(
    graphqlSync({
      schema: buildSchema(fs.readFileSync("schema.graphql", "utf8")),
      source: getIntrospectionQuery(),
    }),
    null,
    2
  )
);
