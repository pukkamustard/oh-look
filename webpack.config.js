const webpack = require('webpack')
const path = require('path')

const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  context: path.join(__dirname, 'src'),
  entry: './index.js',

  output: {
    path: path.join(__dirname, '/build'),
    filename: 'island.js'
  },

  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.elm$/,
        include: [
          path.resolve(__dirname, 'src')
        ],
        use: [{
          loader: 'elm-hot-loader'
        }, {
          loader: 'elm-loader'
        }]
      }
    ]
  },

  plugins: [new HtmlWebpackPlugin({title: 'The Island'})],

  devServer: {
    inline: true
  }

}
