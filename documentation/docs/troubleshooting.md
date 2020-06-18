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

This is because we allow querying union fields only in certain cases. GraphQL
provides the `__typename` field but it's not present in GraphQL introspection
query thus `graphql-ppx` doesn't know that this field exists. To fix your query
simply remove `__typename`. It's added behinds a scene as an implementation
detail and serves us as a way to decide which case to select when parsing your
query result.

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
