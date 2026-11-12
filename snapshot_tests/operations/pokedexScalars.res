module MyQuery = %graphql(
  `
    query pokemon($id: String, $name: String) {
      pokemon(name: $name, id: $id) {
        id
        name
      }
    }
  `
  {schema: "pokedex_schema.json"}
)

module MyQueryDirectives = %graphql(`
    query pokemon($id: String, $name: String)
    @ppxConfig(
      schema: "pokedex_schema.json"
    )
    {
      pokemon(name: $name, id: $id) {
        id
        name
      }
    }
  `)
