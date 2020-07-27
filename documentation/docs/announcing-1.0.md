# Introducing graphql-ppx 1.0

What can we do to stand out as the Reason community? We all know the power of
the typechecker, and how it makes us way more productive, we are shipping less
bugs, and huge refactorings become a piece of cake. And best of all we do it all
without lengthy type annotations.

Also the ergonomics of the language with variants, pattern matching and option
types. It's like having ES2040, but with the bad parts removed.

When building a real app an important building block is data fetching. You need
it for almost any kind of application. There are several ways to go about
fetching data, but there is one protocol that's becoming more and more
ubiquitous, and it looks like it has become the successor of the REST protocol.

GraphQL is a great pairing with Reason, because the protocol is **typed**. This
means that you have a predefined schema of data that your application can fetch.
In principle when we compose a query, given the schema, we know exactly what the
types of the data that is returned is going to be.

In my experience the biggest source of bugs is that the data you get back is not
what you expect. In TypeScript or JavaScript applications you might forget that
a value can be `null` or you might even forgot to include a certain data
requirements in your fragment. Best practice is to
[generate types for your queries](https://www.apollographql.com/docs/react/development-testing/static-typing/).
But now you have to make sure the types always exactly reflect your query, it's
just trading one type of bug for another type of bug, while writing a lot more
code. There are also tools to
[automatically generate](https://github.com/apollographql/apollo-tooling) types
from the queries, mutations and fragments in the source code. But now you need
to import these types from the generated locations, and there is a lot of
annotation you need to do (also nothing prevents you from a wrong annotation).
Every time the schema or your query changes you need to remember to rerun the
generally slow tooling to keep them in sync. The worst part is that you still
need to
[create types](https://hasura.io/learn/graphql/typescript-react-apollo/queries/4-mapping-types/),
for parts of the query that you are manipulating to help the typechecker. My
experience with it has been that the amount of type annotation and work you have
to do to keep all the tooling working in many cases makes you so unproductive
that the better alternative is to live with the unsafety. But that doesn't solve
the largest source of bugs in your apps.

`graphql-ppx` is trying to make working with graphql definitions seamless, as if
it's a new **language primitive**. When you define a graphql query, mutation or
fragment in Reason, `graphql-ppx` will generate all the types. It never needs to
sync up, and is superfast.

You can write a query like this:

```reason
[%graphql {|
  query UserQuery {
    user {
      id
      name
      type
    }
  }
|}];
```

This generates the following module (simplified):

```reason
module type UserQuery = {
  module Raw: {
    type t_user_type = string;
    type t_user = {
      id: string,
      name: Js.Nullable.t(string),
      type: t_user_type
    };
    type t = {user: Js.Nullable.t(t_user)};
  }
  type t_user_type = [
  | ADMIN
  | MEMBER
  ];
  type t_user = {
    id: string,
    name: option(string),
    type: t_user_type
  };
  type t = {user: option(t_user)};
  let parse: Raw.t => t;
  let serialize: t => Raw.t;
}
```

`UserQuery.Raw.t` is what you get back from the server. This is something that
exactly matches the JSON compatible data structure that you receive from the
server if this query successfully executes.

However to make it easier to use in Reason (and more typesafe), we convert the
result to more ergonomic data structures. In this example we convert
`Js.Nullable.t` to option types and the `Enum` type to a polymorphic variant.

Of course you are not actually querying the server yourself. You probably use a
GraphQL client to do that for you.

When you use:

```reason
  let {data} = MyClient.useQuery(module UserQuery)
```

The data is actually the typed reason data structure. So you don't have to worry
about parsing the data yourself at all!

We even provide affordances for clients to extend the generated modules. So with
[`reason-apollo-client`](https://github.com/reasonml-community/reason-apollo-client),
you can just use the following:

```reason
  let { data } = UserQuery.use();
```

## Long in the making

What you saw above is how to use the new version of `graphql-ppx`. This version
has been long in the making. Personally `graphql-ppx` drove me to Reason after
seeing a demo by Sean Grove, as there was no way to deliver this kind of
seamless end-to-end typing in our Typescript application.

As we started using `graphql-ppx` in our application, even as it was already
**way** better than the Typescript situation, there were many things that could
make the experience even better, and they didn't seem to run into fundamental
issues, they just weren't implemented yet.

Most notably record types. Because you are often working with deeply nested
structures, with a lot of option types. Being able to pattern match on the data
is super handy, and makes working with it way easier.

The work for 1.0 started with those record types. And as the data representation
would change it was decided that this would be something that would be a
breaking change, so instead of a minor release, this would be a major release.

This turned out to have major implications on the design and implementation of
`graphql-ppx` itself, because to make this possible we needed to generate types.

This rearchitecture in turn created a lot of opportunities to implement other
things on the wishlist in a much better way. But if we would release these
incrementally, we would have a number of breaking changes along the way. This
would be a pretty bad experience for the users of the library and the authors of
GraphQL clients. This is why we stayed in low-profile beta a little longer to
ship something that is feature complete with no major breaking changes in the
foreseeable future.

## Values

With the new `graphql-ppx` we have certain values that are quite important to
us:

### Great user experience

The most important is that the API of the library should have a great user
experience. This is related to the ability of clients to extend GraphQL
definitions, so we can offer a concise and beautiful API while working with data
without much ceremony.

### Performance

An important value in the Reason community is performance, and we subscribe to
this as well. Even though this project has a fully-fledged GraphQL compiler, it
doesn't have a single dependency. It runs natively and is superfast. When you
compile a project with Bucklescript and `graphql-ppx` time spent in
`graphql-ppx` is only a fraction of the time of compilation, and most of the
extra compilation time is due to `graphql-ppx` producing a lot of code that you
didn't have to write :) We also implement this value in the generated code (we
try to rely on zero-cost bindings as much as possible), and on testing (we have
extensive snapshot and compile testing that runs natively in a few seconds).

## Language primitive

The last value is that we like `graphql-ppx` to be a language primitive for
working with GraphQL inside of Reason. Regardless of which client you are using
(we are actively trying to support all clients), or if you're using it on Reason
Native, OCaml or Bucklescript. With 1.0 we have the fundamentals in place to
support every client. Because some clients have certain functionality that
impact the types generated we sometimes have to add some configuration in the
PPX to make those clients work. We think having a single language primitive that
works across clients (abeit with some configuration), that has the same behavior
is a much better world than having a `ppx` that works slightly different for
each client.

For now we have been focusing on having great out of the box support for the
most popular GraphQL client, Apollo, and have worked closely with Joel on
[`reason-apollo-client`](https://github.com/reasonml-community/reason-apollo-client).
Which are reason bindings for the new Apollo Client 3.0 that take full advantage
of `graphql-ppx` 1.0.

## What's new

### Larger features

- New API, no definition, first class modules and clients can extend definitions
- Using records instead of objects
- Generating types
- Serialization

### Smaller features

- Generating a module signature
- Support for recursive modules
- Raw types
- Leaner parse
- `ppxCustom`
- `ppxAs`
- Better editor support
- Better errors
- Docstrings
- Multiple fragments or operations per extension point
- Configuration in `bsconfig.json`
- Tagged template literal support
- `graphql` extension points in interface files
- Snapshot and compile testing
