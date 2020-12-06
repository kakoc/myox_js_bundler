module.exports = {
	// your root file
	entry: './index.js',
    // target: 'node',

	// output JS bundle to: build/bundle.js
	output: "bundle.js",
module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: 'babel-loader'
      }
    ]
},
 //    node: {
 //  fs: 'empty'
 //    },
    optimization: {
        minimize: false
    },


	// resolve: {
	// 	modulesDirectories: [
	// 		'node_modules'
	// 	]
	// },
};

// module.exports = {
//   entry: './index',
//   output: {
//     filename: 'browser-bundle.js'
//   },
//   devtool: 'source-map',
//   module: {
//     loaders: [
//       {
//         test: /\.js$/,
//         loader: 'babel-loader',
//         query: {
//           presets: ['es2015', 'react']
//         }
//       },
//     ]
//   }
// };
