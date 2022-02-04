module MyQuery = [%graphql
  {|
    query {
        defaultObjectValueOnScalar(
            filter: { some: { json: "value" } }
            arg: {field: "otherValue"}
        )
    }
|}
];
