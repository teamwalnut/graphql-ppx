module Graphql_ppx_runtime = {
  // mock
  let deepMerge = (json1, _) => json1
}

@variadic @taggedTemplate
external graphql: (array<string>, array<string>) => string = "graphql"

module MyQuery = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {templateTag: "graphql"}
)

module MyTypes = {
  type query
}

@variadic @taggedTemplate
external graphql: (array<string>, array<MyTypes.query>) => MyTypes.query = "graphql"

module MyQuery1a = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {templateTag: "graphql", templateTagReturnType: "MyTypes.query"}
)

@variadic @taggedTemplate
external graphql: (array<string>, array<string>) => string = "graphql"

module MyQuery1b = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {templateTag: graphql}
)

module MyQuery2 = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {
    templateTag: "graphql",
    templateTagImport: "graphql",
    templateTagLocation: "gatsby",
  }
)

module MyQuery3 = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {templateTagImport: "graphql", templateTagLocation: "gatsby"}
)

module MyQuery4 = %graphql(
  `
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`
  {templateTagImport: "graphql", templateTagLocation: "gatsby"}
)

%graphql(
  `
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
  query MyQuery5 {
    lists {
      ...ListFragment
    }
  }
  `
  {
    templateTagImport: "graphql",
    templateTagLocation: "gatsby",
    fragmentInQuery: "exclude",
  }
)

%graphql(
  `
  query MyQuery6 {
    lists {
      ...ListFragment
    }
  }
  `
  {
    templateTagImport: "graphql",
    templateTagLocation: "gatsby",
    fragmentInQuery: "include",
  }
)

%graphql(
  `
  query MyQuery7 {
    lists {
      ...ListFragment
    }
  }
  `
  {templateTag: graphql, fragmentInQuery: "exclude"}
)

%graphql(`
  fragment ListFragment8 on Lists @ppxConfig(
    templateTagImport: "gql",
    templateTagLocation: "apollo-client",
    templateTagReturnType: "int",
    templateTagIsFunction: true
  ) {
    nullableOfNullable
    nullableOfNonNullable
  }
  query MyQuery8 @ppxConfig(
    templateTagImport: "gql",
    templateTagLocation: "apollo-client",
    templateTagReturnType: "int",
    templateTagIsFunction: true
  ) {
    lists {
      ...ListFragment8
    }
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
`)
