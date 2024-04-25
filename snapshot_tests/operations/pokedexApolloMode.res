module MyQuery = %graphql(
  `
    {
      pokemon(name: "Pikachu") {
        id
        name
      }
    }
  `
  {schema: "pokedex_schema.json"}
)
