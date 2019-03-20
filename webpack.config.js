const path = require('path');

const HTMLPlugin = require('html-webpack-plugin');
const WebpackNotifier = require('webpack-notifier');

module.exports = {

  mode: 'development',

  context: path.resolve('./src/webapp'),
  entry: './index.js',

  output: {
    path: path.resolve('./target'),
    publicPath: '/assets',
    filename: 'main.js'
  },

  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader:  'elm-webpack-loader',
      options: {
        verbose: true,
        debug: true,
        pathToElm: '/usr/local/bin/elm'
      }
    }],

    noParse: /\.elm$/
  },

  plugins: [
    new WebpackNotifier({
      title: 'Webpack',
      alwaysNotify: true
    }),
    new HTMLPlugin({
      template: path.resolve('./src/webapp/index.html')
    })
  ],

  devtool: 'source-map'

};