---
title: Modeling expected errors
---

If you've got an object which in practice behaves like a variant - like `signUp`
above, where you _either_ get a user _or_ a list of errors - you can add a
`@bsVariant` directive to the field to turn it into a polymorphic variant:

```reason
module SignUpQuery = [%graphql
  {|
mutation($name: String!, $email: String!, $password: String!) {
  signUp(email: $email, email: $email, password: $password) @ppxVariant {
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

let _ =
  Api.sendQuery(
    ~variables=SignUpQuery.makeVariables(
      ~name="My name",
      ~email="email@example.com",
      ~password="secret",
      (),
    ),
    SignUpQuery.definition
  )
  |> Promise.then_(response =>
       (
         switch (response.signUp) {
         | `User(user) => Js.log2("Signed up a user with name ", user.name)
         | `Errors(errors) => Js.log2("Errors when signing up: ", errors)
         }
       )
       |> Promise.resolve
     );

```

This helps with the fairly common pattern for mutations that can fail with
user-readable errors.
