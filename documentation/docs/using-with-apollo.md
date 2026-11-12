---
title: Using with Apollo Client
---

See
[the project documentation](https://github.com/reasonml-community/reason-apollo-client).

There is also the `reason-apollo-hooks` project. This doesn't have official
support for `graphql-ppx` 1.0. Because `graphql-ppx` changed so much and
`apollo-client` 3.0 was released (with many breaking changes). It was decided to
create the new `reason-apollo-client` library (mentioned above). If you have a
large code base, it might make sense to upgrade to the
[`reason-apollo-hooks` PR](https://github.com/reasonml-community/reason-apollo-hooks/pull/117).
that adds support for `graphql-ppx` first. This PR has been used by some larger
companies to gradually upgrade.
