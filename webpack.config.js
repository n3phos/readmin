const path = require('path'),
      package = require('./package.json')

process.env.packageVersion = package.version

module.exports = {
    entry: {
        app: [
            './src/index.js'
        ]
    },

    output: {
        path: path.resolve(__dirname + '/dist'),
        filename: `[name]-v${package.version}.js`,
    },

    module: {
        rules: [{
            test:    /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            loader:  'elm-webpack-loader?verbose=true&warn=true&debug=true'
        }, {
            test:    /\.html$/,
            exclude: /node_modules/,

            loader: (
                'file-loader?name=[name].[ext]!string-replace-loader?search=XXXYYYZZZ&replace=' +
                 package.version
            ),
        }, {
            test:   /\.css$/,
            exclude: /node_modules/,
            loader: 'style-loader!css-loader'
        }],

        noParse: /\.elm$/
    }
}
