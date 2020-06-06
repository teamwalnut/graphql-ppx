---
title: Installation on Reason Native
---

<!-- prettier-ignore -->
:::caution
The Bucklescript version of `graphql-ppx` was almost completely rewritten for the
1.0 release, with many improvements and changes. This documentation will focus
on the API of the bucklescript version. This means that most of the examples
won't apply for the Reason Native version. Please take a look at the
[old documentation](https://github.com/reasonml-community/graphql_ppx/tree/v0.7.1).
At the same time we welcome contributions to modernize the Reason Native version
of `graphql-ppx`
:::

You need to provide the following dependency in your `esy.json` file

```json
{
  "dependencies": {
    "graphql_ppx": "*"
  },
  "resolutions": {
    "graphql_ppx": "reasonml-community/graphql_ppx:esy.json#<use latest stable commit from master>"
  }
}
```

and update your `dune` file:

```
(preprocess (pps graphql_ppx))
```
