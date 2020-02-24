## [0.7.0](https://github.com/reasonml-community/graphql_ppx/compare/v0.6.4...v0.7.0) (2020-02-24)

### Other

- use OMP (ocaml-migrate-parsetree) 1.6.0 ([f7ded8d](https://github.com/reasonml-community/graphql_ppx/commit/f7ded8d2c44bfb53e7308dbcd1c54fa2d1fff7e4))

## [0.6.2](https://github.com/reasonml-community/graphql_ppx/compare/v0.6.1...v0.6.2) (2020-02-18)

### Features

- feat: decoder support on fragment definition ([77d8c45](https://github.com/reasonml-community/graphql_ppx/commit/77d8c452f4aa482585967dd8a33d6ebb262f6e37))
- feat(native): add unified definition ([e240bf3](https://github.com/reasonml-community/graphql_ppx/commit/e240bf3e58b4d14d663cf83d5bb51b40022ba49d))
- chore: support OCaml 4.08 on native ([c533986](https://github.com/reasonml-community/graphql_ppx/commit/c5339868021aa3235e390fc282e5c9d6927cecf3))

### Other

- chore: use internally AST 4.08 ([6acea38](https://github.com/reasonml-community/graphql_ppx/commit/6acea380923c5a698ae151aee93d7f87bea91915))

## [0.6.1](https://github.com/reasonml-community/graphql_ppx/compare/v0.5.0...v0.6.1) (2020-01-23)

### Features

- Lean parse available under experimental `-lean-parse` feature flag ([6e7224b](https://github.com/reasonml-community/graphql_ppx/commit/6e7224ba9789bc67a68aa9566e295eff70855dd0))

### Other

- Update repository to name and organization change ([5df9be8](https://github.com/reasonml-community/graphql_ppx/commit/5df9be88ef5bd7be77df551265834cb1bc31a011))

## [0.5.0](https://github.com/reasonml-community/graphql_ppx/compare/v0.4.9...v0.5.0) (2020-01-11)

### Features

- Support configuration via ppx-flags, drop env based one ([3ed986e](https://github.com/reasonml-community/graphql_ppx/commit/3ed986e7cf020e751ce93a46896f45c33e52c860))
- Move pipeline to one file, add esy cache ([b9517da9](https://github.com/reasonml-community/graphql_ppx/commit/b9517da9d74e8e90b83808b6dfcc520f97fca7ea))

## [0.4.9](https://github.com/reasonml-community/graphql_ppx/compare/v0.4.6...v0.4.9) (2020-01-06)

### Bug Fixes

- ci: build linux binary on alpine with static linking ([9a4f31b](https://github.com/reasonml-community/graphql_ppx/commit/9a4f31b76350bf73e108d31a5d6a75dc9a681238))

## [0.4.1](https://github.com/reasonml-community/graphql_ppx/compare/v0.4.0...v0.4.1) (2019-12-04)

### Bug Fixes

- one field query with bs record ([e231d19](https://github.com/reasonml-community/graphql_ppx/commit/e231d1970b69f175400a324e2542a4748588b34b))
- **bucklescript:** test suite upgrade and how to run tests documentation improved ([19db453](https://github.com/reasonml-community/graphql_ppx/commit/19db453aef42f354f2e8f5cd532002ebfa66c22c))

### Features

- Multiple schemas support ([467d557](https://github.com/reasonml-community/graphql_ppx/commit/467d55799771825b3fcdd3ccd4098c3a021328e9))

# [0.4.0](https://github.com/reasonml-community/graphql_ppx/compare/v0.3.5...v0.4.0) (2019-11-25)

### Features

- Unified definition - (parse, query, combineVariables) tuple to improve client side usage [8de2419](https://github.com/reasonml-community/graphql_ppx/commit/8de241902cb660c830659ea659f56fce92ad423c)

## [0.3.5](https://github.com/reasonml-community/graphql_ppx/compare/v0.3.3...v0.3.5) (2019-11-23)

### Bug Fixes

- explicit annotate custom scalar as Yojson.Basic.t for return type magic ([1fa67ae](https://github.com/reasonml-community/graphql_ppx/commit/1fa67ae))

### Features

- add `makeVariables` function ([667673c](https://github.com/reasonml-community/graphql_ppx/commit/667673c7c815eb53ea26f7d9d06544439eae0f28))

## [0.3.3](https://github.com/reasonml-community/graphql_ppx/compare/v0.3.2...v0.3.3) (2019-11-23)

### Bug Fixes

- remove `@bsField` directive from query output ([b3b17f3](https://github.com/reasonml-community/graphql_ppx/commit/b3b17f3))
- **bucklescript:** polymorphic comparison on Js.Json.t ([d1897c2](https://github.com/reasonml-community/graphql_ppx/commit/d1897c2))

## [0.3.2](https://github.com/reasonml-community/graphql_ppx/compare/v0.3.1...v0.3.2) (2019-10-21)

### Bug Fixes

- improve schema reading (handle case where data is not in json schema) ([9324ba2](https://github.com/reasonml-community/graphql_ppx/commit/9324ba232385540b61f485064ee09b1e49929146))

## [0.3.1](https://github.com/reasonml-community/graphql_ppx/compare/0.2.0...v0.3.1) (2019-10-19)

### Bug Fixes

- **tests:** change how tests are run ([8295a7e](https://github.com/reasonml-community/graphql_ppx/commit/8295a7e))

### Features

- **validation:** no undefined variables rule ([5d8772a](https://github.com/reasonml-community/graphql_ppx/commit/5d8772a))
