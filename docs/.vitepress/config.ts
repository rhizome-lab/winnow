import { defineConfig } from 'vitepress'
import { withMermaid } from 'vitepress-plugin-mermaid'

export default withMermaid(
  defineConfig({
    vite: {
      optimizeDeps: {
        include: ['mermaid'],
      },
    },
    title: 'Reincarnate',
    description: 'Legacy software lifting framework',

    base: '/reincarnate/',

    themeConfig: {
      nav: [
        { text: 'Guide', link: '/introduction' },
        { text: 'Targets', link: '/targets/' },
        { text: 'RHI', link: 'https://docs.rhi.zone/' },
      ],

      sidebar: {
        '/': [
          {
            text: 'Guide',
            items: [
              { text: 'Introduction', link: '/introduction' },
              { text: 'Getting Started', link: '/getting-started' },
              { text: 'Architecture', link: '/architecture' },
            ]
          },
          {
            text: 'Targets',
            items: [
              { text: 'Overview', link: '/targets/' },
              { text: 'Flash', link: '/targets/flash' },
              { text: 'Director', link: '/targets/director' },
              { text: 'Visual Basic 6', link: '/targets/vb6' },
              { text: 'Game Engines', link: '/targets/game-engines' },
            ]
          },
          {
            text: 'Design',
            items: [
              { text: 'Philosophy', link: '/philosophy' },
              { text: 'Tier 1 vs Tier 2', link: '/tiers' },
              { text: 'Persistence & Saving', link: '/persistence' },
            ]
          },
        ]
      },

      socialLinks: [
        { icon: 'github', link: 'https://github.com/rhi-zone/reincarnate' }
      ],

      search: {
        provider: 'local'
      },

      editLink: {
        pattern: 'https://github.com/rhi-zone/reincarnate/edit/master/docs/:path',
        text: 'Edit this page on GitHub'
      },
    },
  }),
)
