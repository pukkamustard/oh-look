const webpack = require('webpack')
const path = require('path')

const CopyWebpackPlugin = require('copy-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: './src/index.js',

  output: {
    path: path.join(__dirname, '/docs'),
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
  }),
    new CopyWebpackPlugin([{from: 'assets/', to: 'assets/'}])
  ],

  devServer: {
    inline: true
  }

}
