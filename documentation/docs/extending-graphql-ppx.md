---
title: Extending graphql-ppx
---

Graphql-ppx has a way to extend the basic build in functionality of the modules
that are generated. By default Graphql-ppx does the following

- Create types (`Query.t`)
- Parses raw data to ReasonML data (`parse`)
- Serializes ReasonML data to raw data (`serialize`)
- Generates functions to generate variables and input objects (`makeVariables`)

You usually want to do more than just that, mostly involving a GraphQL client:

- `use`-ing the query with a React-hook
- Executing the query
- ...

Because Graphql-ppx is client independent we provide a way for clients to extend
the modules that Graphql-ppx generates.

When zooming in on the React case, it would be great to have a `use` function
(hook) on `Query`, that would provide us with the query result in a react
component.

In ReasonML the way to extend modules is by using
[functors](https://2ality.com/2018/01/functors-reasonml.html).

Let's extend the following query:

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

To build a functor, we first need to declare the module type that this functor
will apply to. In this case we would like it to work for any query, so we need
to provide the types, values and functions that we need to use in the extension.

```reason
module type GraphQLQuery = {
  module Raw: {
    type t;
  };
  type t;
  let query: string;
  /* this just makes sure it's just a type conversion, and no function have
     to be called */
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  let parse: Raw.t => t;
};
```

The above is all we need for now. This means we have access to the types, the
`query` string and the `parse` function. To extend `UserQuery` we can apply a
functor as follows:

```reason
module ExtendedUserQuery = {
  include UserQuery;
  include ExtendQuery(UserQuery);
};
```

Now we have `ExtendedUserQuery` that has the same functionality as UserQuery but
adds anything the `ExtendQuery` might add to this.

How to go about building `ExtendQuery`? Let's imagine we have a `GraphQLClient`
with a `use` function, this function returns `None` if the data is loading, and
a result type if the data has loaded. Let see how a basic version of this
functor looks:

```reason
module ExtendQuery = (M: GraphQLQuery) => {
  let use = () => {
    switch (GraphQLClient.use(M.query)) {
      | None => None
      | Some(Ok(data)) => Some((Ok(data->M.cast->M.parse))
      | Some(Error(errors)) => Some(Error(errors))
    }
  }
}
```

Now we can call `ExtendedQuery.use` in a React component. If the data comes back
(without errors), we pattern match on the result, and call `cast` to type cast
the JSON data to `Raw.t`, and then `parse` to parse it to the ReasonML data
types.

There are many more things we can do to make this a great extension, such as
memoizing the data parsing, handling variables, and all other options that your
GraphQL client might provide.

We now can use `ExtendQuery` to extend all our Graphql-ppx queries. However,
this adds quite a lot of boilerplate, because we have to repeat these lines of
code for each query:

```reason
module ExtendedUserQuery = {
  include UserQuery;
  include ExtendQuery(UserQuery);
};
```

This is why there is a way to let `graphql-ppx` do this work for you. You can do
it on a query basis:

```reason
[%graphql {|
  query UserQuery @ppxConfig(extend: "ExtendQuery") {
    user {
      id
      name
    }
  }
|}
];
```

Most probably you'd want to do this for each query in your project. This is
possible by using the BuckleScript configuration file (`bsconfig.json`):

```json
{
  "graphql": {
    "extendQuery": "ExtendQuery",
    "extendQueryNoRequiredVariables": "ExtendQueryNoRequiredVars",
    "extendMutation": "ExtendMutation",
    "extendMutationNoRequiredVariables": "ExtendMutationNoRequiredVars",
    "extendSubscription": "ExtendSubscription",
    "extendSubscriptionNoRequiredVariables": "ExtendSubscriptionNoRequiredVars",
    "extendFragment": "ExtendFragment"
  }
}
```
