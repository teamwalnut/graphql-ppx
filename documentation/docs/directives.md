---
title: Directives
---

`graphql-ppx` is offering some directives to customize how the ppx is handling
operations or fragments.

## `arguments`

See [Fragment](fragment.md)

## `argumentDefinitions`

See [Fragment](fragment.md)

## `ppxAs`

If you add the directive ppxAs to a GraphQL object, it will be casted using that
record. Be careful this is unsafe and should only be used as an excape hatch.

```reason {8}
type user = {
  id: string,
  name: option(string),
}

[%graphql {|
  query {
    @ppxAs(type: "user")
    user {
      id
      name
    }
  }
 |}
```

## `ppxCustom`

`ppxCustom` is a great way to transform the output of a Query to a value that is
more useful. For instance,
[custom scalars](https://graphql.org/learn/schema/#scalar-types) are always
`Js.Json.t`, we could use custom scalars for instance for dates. To make them
directly useful in our apps, using the `ppxCustom` directive we can make a
little annotation to convert this to an actual date. Here is an example.

```reason {15}
module MyDate = {
  type t = option(Js.Date.t);
  let parse = json => switch(json->Js.Json.decodeString) {
    | Some(str) => str->Js.Date.fromString
    | None => None
  }
  let serialize = date => date->Js.Date.toString->Js.Json.string
}

[%graphql {|
  query UserQuery {
    user {
      id
      name
      birthday @ppxCustom(module: "MyDate")
    }
  }
 |}
];
```

As you can see we need to define a type `t` and two functions, `parse` and
`serialize`. These two functions are to go from `Js.Json.t` to `t` and how to go
from `t` back to `Js.Json.t`.

In [customFields](/docs/configuration#customfields) in `bsconfig.json` you can
actually configure certain types (such as the custom date type, that **always**
gets converted, so you never have to use the `@ppxCustom` annotation for them.

It's even possible to apply this to other than scalars. Any GraphQL value can
"customized". The problem is that if you'd like to transform a value from a type
defined **in** the generated module, to your own type, this sets up a recursive
relationship between those module. In Reason you need to explicitly mark these
two modules as recursive. However it is possible to use the `%graphql` extension
point as a recursive module. In below example we transforming a user record into
a customized record:

```reason {3}
module UserQuery = [%graphql {|
  query {
    user @ppxCustom(module: "User") {
      id
      name
    }
  }
 |} and User: {
   type t = {
     nameAndId: string
   };
   let parse: UserQuery.t_user => t;
   let serialize: t => UserQuery.t_user;
 } = {
   type t = {
     nameAndId: string
   };
   let parse = user => {
     nameAndId: switch (user.name) {
       None => "Noname" ++ user.id
       Some(name) => name ++ user.id
     }
   };
 }
```

## `ppxOmitFutureValue`

For enums and unions we always generate a `#FutureAddedValue(_)` variant to make
sure that your app doesn't raise exceptions if enums are unions are added to the
GraphQL schema at some later point in time. This is necessary to make sure your
app is typesafe and doesn't run into runtime exceptions. If you are absolutely
sure that the schema won't change, you can add this directive to remove this
extra variant.

## `ppxVariant`

If you've got an GraphQL object which in practice behaves like a variant, where
you _either_ get a user _or_ a list of errors - you can add a `@bsVariant`
directive to the field to turn it into a polymorphic variant:

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

let (mutation, _) =
  SignUpQuery.useWithVariables(SignUpQuery.makeVariables(
    ~name="My name",
    ~email="email@example.com",
    ~password="secret",
    (),
  ))

mutation()
|> Promise.then_(data =>
  switch (data.signUp) {
  | `User(user) => Js.log2("Signed up a user with name ", user.name)
  | `Errors(errors) => Js.log2("Errors when signing up: ", errors)
  }
  |> Promise.resolve
);
```

This helps with the fairly common pattern for mutations that can fail with
user-readable errors.

## `ppxField`

If you'd like to specify which field the fragment uses in the record, you can do
it by using `@ppxField`. (By default it used the name of the fragment with the
first letter decapitalized.)

```reason
[%graphql
  {|
  query MyQuery {
    lists {
      ...ListFragment @ppxField(name: "myFragment")
    }
  }
|}
];
```

## `skip`

The standard [skip directive](https://graphql.org/learn/queries/#directives) is
also supported within GraphQL ppx. This makes sure the field is always an option
type.

## `include`

The standard [include directive](https://graphql.org/learn/queries/#directives)
is also supported within GraphQL ppx. This makes sure the field is always an
option type.

## `ppxObject` (deprecated)

Convert a specific field to a record when graphql-ppx is generating objects
instead of record. It is marked as deprecated because in a future release
support for objects is going to be removed.

## `ppxRecord` (deprecated)

If the default is to generate objects, using this directive you can force a
record to be generated. It is marked as deprecated because in a future release
support for objects is going to be removed.

## `ppxDecoder` (deprecated)

This was the old name for `ppxCustom`
