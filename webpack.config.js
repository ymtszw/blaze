const path = require('path')

const BASE = path.join(__dirname, 'dist')
const EXCLUDED = [
  /elm-stuff/,
  /node_modules/,
]

module.exports = () => {
  const node_env = (process.env.NODE_ENV == 'prod') ? 'prod' : 'dev'

  return {
    entry: {
      igniter: ['./src/igniter.js'], // Crawler
      worker: ['./src/worker.js'], // Worker for Qiita article
    },

    target: 'node',

    output: {
      path: BASE,
      filename: '[name].js',
    },

    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: EXCLUDED,
          use: {
            loader: 'elm-webpack-loader',
            options: {
              debug: node_env == 'dev',
              warn: true,
            },
          },
        },
      ],
    },
  }
}
