/**
 * Mismatched simple type
 */
module MyQuery = %graphql(`
    query {
        invalidObjectValues(
            nonRec: {
                nonNullableField: 5
                nullableArray: [""]
                field: ""
                enum: FIRST
            }
            rec: {
                otherField: ""
                inner: {
                    otherField: ""
                }
                enum: FIRST
            }
        )
    }
`)
