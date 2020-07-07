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
