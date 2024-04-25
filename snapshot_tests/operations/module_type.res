module MyQuery: {
  type t
} = %graphql(`
  {
    variousScalars  {
      string
    }
  }
`)

module MyQuery2: {
  type t_variousScalars
  type t = {variousScalars: t_variousScalars}
} = %graphql(
  `
  {
    variousScalars  {
      string
    }
  }
`
  {records: true}
)

module rec MyQueryRecursive = %graphql(
  `
  query {
    nestedObject @ppxCustom(module: "VariousScalars") {
      inner {
        inner {
          field
        }
      }
    }
  }
`
  {records: true, apolloMode: false}
)

and VariousScalars: {
  type t
  let parse: MyQueryRecursive.t_nestedObject => t
  let serialize: t => MyQueryRecursive.t_nestedObject
} = {
  type t = {otherInner: option<MyQueryRecursive.t_nestedObject_inner>}
  let parse = (nestedObject: MyQueryRecursive.t_nestedObject) => {
    otherInner: nestedObject.inner,
  }
  let serialize: t => MyQueryRecursive.t_nestedObject = t => {
    inner: t.otherInner,
  }
}
