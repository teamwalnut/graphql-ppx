// Test for GraphQL fields with illegal ReScript names (starting with uppercase)
// The field "SSN" should be escaped as \"SSN" in the generated ReScript code

module IllegalNameQuery = %graphql(`
  query IllegalNameQuery {
    variousScalars {
      SSN
    }
  }
`)

let getSsn = (data: IllegalNameQuery.t) => {
  data.variousScalars.\"SSN"
}
