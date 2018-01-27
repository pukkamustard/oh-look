const webpack = require('webpack')
const path = require('path')

const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: './src/index.js',

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

  plugins: [new HtmlWebpackPlugin({
    template: 'src/index.html'
  })],

  devServer: {
    inline: true
  }

}
