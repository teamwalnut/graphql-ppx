module MyQuery = %graphql(`
  query ($opt: CustomScalar, $req: CustomScalar!) {
    customScalarField(argOptional: $opt, argRequired: $req) {
      nullable
      nonNullable
    }
  }
`)
