[%graphql
  {|
  fragment Screen_Model on Screen {
    id
  }

  fragment DemoVersion_Model on DemoVersion {
    screens {
      ...Screen_Model
    }
  }
|}
];
