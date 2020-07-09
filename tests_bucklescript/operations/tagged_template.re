module GraphQL_PPX = {
  // mock
  let deepMerge = (json1, _) => {
    json1;
  };
};

module MyQuery = [%graphql
  {|
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
|};
  {templateTag: "graphql"}
];

module MyTypes = {
  type query;
};

module MyQuery1a = [%graphql
  {|
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
|};
  {templateTag: "graphql", templateTagReturnType: "MyTypes.query"}
];

module MyQuery1b = [%graphql
  {|
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
|};
  {templateTag: graphql}
];

module MyQuery2 = [%graphql
  {|
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
|};
  {
    templateTag: "graphql",
    templateTagImport: "graphql",
    templateTagLocation: "gatsby",
  }
];

module MyQuery3 = [%graphql
  {|
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
|};
  {templateTagImport: "graphql", templateTagLocation: "gatsby"}
];

module MyQuery4 = [%graphql
  {|
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
|};
  {templateTag: graphql, templateTagLocation: "gatsby"}
];

[%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
  query MyQuery5 {
    lists {
      ...ListFragment
    }
  }
  |};
  {
    templateTag: graphql,
    templateTagLocation: "gatsby",
    fragmentInQuery: "exclude",
  }
];

[%graphql
  {|
  query MyQuery6 {
    lists {
      ...ListFragment
    }
  }
  |};
  {
    templateTag: graphql,
    templateTagLocation: "gatsby",
    fragmentInQuery: "include",
  }
];

[%graphql
  {|
  query MyQuery7 {
    lists {
      ...ListFragment
    }
  }
  |};
  {templateTag: graphql, fragmentInQuery: "exclude"}
];
