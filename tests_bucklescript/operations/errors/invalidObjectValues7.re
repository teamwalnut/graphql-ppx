/**
 * This should not throw as it's the right object shape inside the list
 */
module MyQuery = [%graphql
  {|
    query {
        invalidObjectValuesLst(
            embeddedList: [{field: ""}]
        )
    }
|}
];
/**
 * This should throw as it's not the right shape
 */
module MyQuery2 = [%graphql
  {|
    query {
        invalidObjectValuesLst(
            embeddedList: [{field: 5}]
        )
    }
|}
];
