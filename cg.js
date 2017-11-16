#!/usr/bin/env node
'use strict';

const fs = require('fs');

const yaml = require('js-yaml');
const swagger2openapi = require('swagger2openapi');

const processor = require('./index.js');

var argv = require('yargs')
    .usage('cg [options] {configName} {openapi-definition}')
    .boolean('debug')
    .alias('d','debug')
    .describe('debug','Turn on debugging information in the model')
    .boolean('lint')
    .alias('l','lint')
    .describe('lint','Lint input definition')
    .boolean('verbose')
    .describe('verbose','Increase verbosity')
    .alias('v','verbose')
    .version()
    .argv;

let configName = argv._[0] || 'nodejs';
let config = require('./configs/'+configName+'.json');
if (argv.verbose) {
    config.defaults.verbose = true;
    console.log('Loaded config '+configName);
}
if (argv.lint) config.defaults.lint = true;
if (argv.debug) config.defaults.debug = true;

let defName = argv._[1] || './defs/petstore3.json';
let s = fs.readFileSync(defName,'utf8');
let o = yaml.safeLoad(s, { json: true } );
if (argv.verbose) console.log('Loaded definition '+defName);

if (o.openapi) {
    processor.main(o,config,configName);
}
else {
    swagger2openapi.convertObj(o,{patch:true,warnOnly:true,direct:true},function(err,openapi){
        if (argv.verbose) console.log('Converting OpenAPI 2.0 definition');
        config.defaults.swagger = o;
        processor.main(openapi,config,configName);
    });
}
