module.exports = {
  title: "GraphQL PPX",
  tagline: "GraphQL infrastructure for ReasonML",
  url: "https://graphql-ppx.com",
  baseUrl: "/",
  favicon: "img/favicon.png",
  organizationName: "reasonml-community", // Usually your GitHub org/user name.
  projectName: "graphql_ppx", // Usually your repo name.
  themeConfig: {
    hideOnScroll: true,
    prism: {
      theme: require("prism-react-renderer/themes/github"),
      darkTheme: require("prism-react-renderer/themes/oceanicNext"),
    },
    navbar: {
      title: "Graphql PPX",
      logo: {
        alt: "GraphQL Logo",
        src: "img/logo.svg",
        srcDark: "img/logo.svg",
      },
      links: [
        { to: "docs/introduction", label: "Docs", position: "left" },
        {
          href: "https://github.com/reasonml-community/graphql_ppx",
          label: "GitHub",
          position: "right",
        },
      ],
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Docs",
          items: [
            {
              label: "Docs",
              to: "docs/introduction",
            },
          ],
        },
      ],
    },
  },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          sidebarPath: require.resolve("./sidebars.js"),
          admonitions: {
            icons: "emoji",
            infima: false,
          },
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      },
    ],
  ],
};
