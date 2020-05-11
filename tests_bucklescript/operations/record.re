type scalars = {
  string,
  int,
};

type dog = {
  name: string,
  barkVolume: float,
};

type oneFieldQuery = {nullableString: option(string)};

module MyQuery = [%graphql
  {|
  {
    variousScalars @bsRecord @ppxAs(type: "scalars") {
      string
      int
    }
  }
|}
];

module OneFieldQuery = [%graphql
  {|
  {
    variousScalars @bsRecord {
      nullableString
    }
  }
|}
];

module ExternalFragmentQuery = [%graphql
  {|
  fragment Fragment on VariousScalars @bsRecord {
    string
    int
  }
  {
    variousScalars {
      ...Fragment
    }
  }
|}
];

module InlineFragmentQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog @bsRecord {
        name
        barkVolume
      }
    }
  }
|}
];

module UnionExternalFragmentQuery = [%graphql
  {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }
  {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}
];
