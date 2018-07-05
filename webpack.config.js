'use strict';
const webpack = require('webpack');

module.exports = {
    mode: 'production',
    performance: { hints: false },
    node: {
        fs: "empty"
    },
    entry: {
        openapiCodeGen: './index.js'
    },
    output: {
        filename: '[name].min.js', // Template based on keys in entry above
        library: '[name]', // was constant
        libraryTarget: 'var'
    },
    plugins: []
};

