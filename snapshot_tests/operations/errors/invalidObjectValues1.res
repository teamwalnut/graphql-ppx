/**
 * Nested objects
 */
module MyQuery = %graphql(`
    query {
        invalidObjectValues(
            nonRec: {
                nonNullableField: "!"
                nullableArray: [""]
                field: ""
                enum: FIRST
            }
            rec: {
                otherField: ""
                inner: {
                    otherField: ""
                    inner: {
                        otherField: 5
                    }
                }
                enum: FIRST
            }
        )
    }
`)
