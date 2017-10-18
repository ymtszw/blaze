const path = require('path');

const BASE = path.join(__dirname, 'dist');
const EXCLUDED = [
  /elm-stuff/,
  /node_modules/,
]

module.exports = () => {
  const node_env = (process.env.NODE_ENV == 'prod') ? 'prod' : 'dev'

  return {
    entry: {blaze: ['./src/blaze.js']},

    devServer: {
      contentBase: BASE,
      watchContentBase: true,
      compress: true,
      stats: {
        colors: true,
        depth: true,
      },
    },

    target: 'node',

    output: {
      path: BASE,
      filename: '[name].js',
    },

    module: {
      rules: [
        {
          test: /\.html$/,
          exclude: EXCLUDED,
          use: {
            loader: 'file-loader',
            options: {name: '[name].[ext]'},
          },
        },
        {
          test: /\.css$/,
          exclude: EXCLUDED,
          use: [
            {loader: 'style-loader'},
            {
              loader: 'css-loader',
              options: {importLoaders: 1},
            },
            {loader: 'postcss-loader'},
          ],
        },
        {
          test: /\.elm$/,
          exclude: EXCLUDED,
          use: {
            loader: 'elm-webpack-loader',
            options: {debug: node_env == 'dev'},
          },
        },
      ],
    },
  }
}
