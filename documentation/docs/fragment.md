---
title: Fragment
---

Records in ReasonML are nominally typed. Even if a records contains exactly the
same fields as another record, it will be seen as a different type, and they are
not compatible. That means that if you want to create an `createAvatar` function
for a `User`, you'd be able to accept for instance `UserQuery.t_user` as an
argument. That's all great, but what if you have another query where you also
would like to create an avatar. In most cases Fragments are the solution here.

With fragments you can define reusable pieces that can be shared between
queries. You can define a fragment in the following way

```reason
[%graphql {|
  fragment Avatar_User on User {
    id
    name
    smallAvatar: avatar(pixelRatio: 2, width: 60, height: 60) {
      url
    }
  }

  query UserQuery {
    user {
      id
      role
      ...Avatar_User
    }
  }
|}]
```

This generates the module `Avatar_User` as the fragment. The `createAvatar` can
now accept `Avatar_User.t` which include all the fields of the fragment.

How to we get this from the query? When you use the spread operator with the
module name, an extra field is created on the `t_user` record with the name
`avatar_User` (same as the fragment module name but with a lowercase first
letter). This is the value that has the type `Avatar_User.t` containing all the
necessary fields.

If you want to change the default name of the fragment you can use a GraphQL
alias (`avatarFragment: ...AvatarUser`).

When there is just the fragment spread and no other fields on an object, there
is no special field for the fragment necessary. So if this is the query:

```reason
[%graphql {|
  query UserQuery {
    user {
      ...Avatar_User
    }
  }
|}]
```

Then `user` will be of the type `Avatar_User.t`.

#### Referencing fragments from other modules

Often, you'll want to reference a fragment in a different module, so that you
can compose it into a query that's defined with a different component. When
doing so, you must use the same name for the fragment in the GraphQL document as
you gave it where you defined it - you can't rename it.

One way to achieve this is to define a new aliased module with the same name:

```reason
module Avatar_User = Avatar.Avatar_User;

[%graphql {|
  query UserQuery {
    user {
      ...Avatar_User
    }
  }
|}]
```

Alternatively, you can also `open` the module containing the fragment where it's
being used, and reference it like this:

```reason
open Avatar;

[%graphql {|
  query UserQuery {
    user {
      ...Avatar_User
    }
  }
|}]
```

#### Variables within fragments

Sometimes fragments need to accept variables. Take our previous fragment. If we
would like to pass the pixelRatio as a variable as it might vary per device. We
can do this as follows:

```reason
[%graphql {|
  fragment Avatar_User on User @argumentDefinitions(pixelRatio: {type: "Float!"}) {
    id
    name
    smallAvatar: avatar(pixelRatio: $pixelRatio, width: 60, height: 60) {
      url
    }
  }

  query UserQuery($pixelRatio: Float!) {
    user {
      id
      role
      ...Avatar_User @arguments(pixelRatio: $pixelRatio)
    }
  }
|}]
```

To be able to typecheck these variables and make sure that the types are
correct, there are no unused variables or variables that are not defined, we
introduce two directives here `argumentDefinitions` and `arguments`, these are
taken from
[Relay](https://relay.dev/docs/en/fragment-container#argumentdefinitions). But
they have nothing to do with the relay client (we just re-use this convention).

Note that you cannot rename variables in the `@arguments` directive so the name
of the variable and the name of the key must be the same. This is because
`graphql-ppx` does not manipulate variable names and just makes use of the fact
that fragments can use variables declared in the query.

There is a compile error raised if you define variables that are unused. If you
(temporarily) want to define unused variables you can prepend the variable name
with an underscore.

#### `ppxAs`

An ecape hatch for when you don't want `graphql-ppx` to create a record type,
you can supply one yourself. This also makes reusability possible. We recommend
fragments however in most cases as they are easier to work, are safer and don't
require defining separate types.

```reason
type t_user = {
  id: string
  role: string
}

[%graphql {|
  query UserQuery {
    user @ppxAs(type: "t_user") {
      id
      role
    }
  }
|}]
```
