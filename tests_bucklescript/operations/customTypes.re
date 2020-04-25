module MyQuery = [%graphql
  {|
  query {
    customFields {
        currentTime
        favoriteColor
        futureTime
        nullableColor @ppxDecoder(module: "GraphqlHelpers.DateTime")
    }
  }
|}
];