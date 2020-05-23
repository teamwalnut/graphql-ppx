---
title: Installation on Bucklescript
---

First, add it to you dependencies using `npm` or `yarn`:

```sh
yarn add @baransu/graphql_ppx_re@next --dev
# or
npm install @baransu/graphql_ppx_re@next  --saveDev
```

Second, add it to `ppx-flags` in your `bsconfig.json`:

```json
"ppx-flags": ["@baransu/graphql_ppx_re/ppx"]
```
