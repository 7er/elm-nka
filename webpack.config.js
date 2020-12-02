const path = require("path");
const webpack = require("webpack");


const PATHS = {
  app: path.join(__dirname, 'src/index.js'),
  build: path.join(__dirname, 'dist'),
};

const makeConfig = (isDevelopment) => {
  return {
    entry: {
      app: PATHS.app
    },
    output: {
      path: PATHS.build,
      filename: '[name].js',
    },
    plugins : [
    ],

    module: {
      rules: [
        {
          test: /\.(css|scss)$/,
          use: [
            'style-loader',
            'css-loader',
            'sass-loader',
          ]
        },
        {
          test:    /\.html$/,
          exclude: /node_modules/,
          loader:  'file-loader?name=[name].[ext]',
        },
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: 'elm-assets-loader',
              options: {
                module: 'Assets',
                tagger: 'Image'
              }
            },            
            {
              loader:  'elm-webpack-loader',
              options: {
                verbose: true,
                warn: true,
                debug: isDevelopment,
              }
            }
          ]
        },
        {
          test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          loader: 'url-loader?limit=10000&mimetype=application/font-woff',
        },
        {
          test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          loader: 'file-loader',
        },
        {
          test: /\.(jpe?g|png|gif|svg)$/i,
          loader: 'file-loader',
          options: {
            name: '[name]-[hash].[ext]'
          }
        },
        {
          test: /favicon.ico$/,
          loader: 'file-loader',
          options: {
            name: '[name].[ext]'
          }
        },
        {
          test: /\.(pdf)$/,
          loader: 'file-loader',
        },
      ],

      //noParse: /\.elm$/,
    },

    devServer: {
      inline: true,
      stats: { colors: true },
    },
  }
};

module.exports = (env) => {
  console.log("env", env);
  return makeConfig(env == 'development');
}
