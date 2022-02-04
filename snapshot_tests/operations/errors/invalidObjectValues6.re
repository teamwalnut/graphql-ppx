/**
 * This should not throw as it's an empty list
 */
module MyQuery = [%graphql
  {|
    query {
        invalidObjectValuesLst(
            embeddedList: []
        )
    }
|}
];
/**
 * This should throw as it's not a list
 */
module MyQuery2 = [%graphql
  {|
    query {
        invalidObjectValuesLst(
            embeddedList: {field: ""}
        )
    }
|}
];
