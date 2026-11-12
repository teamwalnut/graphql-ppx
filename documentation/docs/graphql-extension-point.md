---
title: GraphQL extension point
---

`graphql-ppx` introduces the a GraphQL extension point in the ReasonML (or
OCaml) language.

It allows you to write GraphQL [definitions](definition) inside of the language
and `graphql-ppx` takes care of the following for you:

- Generates the data types of a GraphQL query, mutation, subscription, or
  fragment
- Creates `parse` and `serialize` functions to convert a JSON data response to a
  fully typed Reason data structure (and the other way around)
- Generates the data types of GraphQL variables

You define a query like this:

```reason
[%graphql {|
  query ExampleQuery = {
    myQuery {
      myField
    }
  }
|}]
```

This will generate the the `ExampleQuery` module.

When you'd like to alias the module to a different name you can do that like
this:

```reason
module OtherName = [%graphql {|
  query ExampleQuery = {
    myQuery {
      myField
    }
  }
|}]
```

You can also have more definitions inside of the GraphQL extension point:

```reason
[%graphql {|
  query ExampleQuery = {
    myQuery {
      myField
    }
  }
  query OtherQuery = {
    myOtherQuery {
      myOtherField
    }
  }
|}]
```
