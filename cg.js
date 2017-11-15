#!/usr/bin/env node
'use strict';

const fs = require('fs');

const yaml = require('js-yaml');
const swagger2openapi = require('swagger2openapi');

const processor = require('./index.js');

let configName = process.argv[2] || 'nodejs'; // TODO args parsing with yargs
let config = require('./configs/'+configName+'.json');

let defName = process.argv[3] || './defs/petstore3.json';
let s = fs.readFileSync(defName,'utf8'); // TODO conversion of openapi 2.0 definitions
let o = yaml.safeLoad(s, { json: true } );

if (o.openapi) {
    processor.main(o,config,configName);
}
else {
    swagger2openapi.convertObj(o,{patch:true,warnOnly:true,direct:true},function(err,openapi){
        processor.main(openapi,config,configName);
    });
}
