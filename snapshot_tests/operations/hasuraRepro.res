%graphql(`
    fragment Dog on Dog {
      name
    }
    query HasuraRepro($blockNumber: Int, $type: String) {
      hasuraRepro(orderBy: [{id: desc}], block: { number: $blockNumber, type: $type }) {
        ...Dog
      }
    }
  `)

let _ = HasuraRepro.makeVariables(~blockNumber=1, ~type_="1")
