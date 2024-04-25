module MyQuery = %graphql(`
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
`)

%graphql(`
    query Test($input: problem_input!) {
      recursiveRepro(input: $input)
    }
  `)
