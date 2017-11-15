#!/usr/bin/env node
'use strict';

const fs = require('fs');
const util = require('util');

const yaml = require('js-yaml');
const mkdirp = require('mkdirp');

const adaptor = require('./adaptor.js');
const renderer = require('./index.js');

let s = fs.readFileSync('./specs/petstore3.json','utf8'); // TODO conversion of openapi 2.0 definitions
let o = yaml.safeLoad(s, { json: true } );

let configName = process.argv[2] || 'nodejs'; // TODO args parsing with yargs
let config = require('./configs/'+configName+'.json');

let model = adaptor.transform(o, config.defaults);

for (let p in config.partials) {
    let partial = config.partials[p];
    config.partials[p] = fs.readFileSync('./templates/'+configName+'/'+partial,'utf8');
}

let actions = [];
for (let t in config.transformations) {
    let tx = config.transformations[t];
    tx.template = fs.readFileSync('./templates/'+configName+'/'+tx.input,'utf8');
    actions.push(tx);
}

mkdirp('./out/'+configName,function(){
    if (config.directories) {
        for (let directory of config.directories) {
            mkdirp.sync(directory);
        }
    }
    for (let action of actions) {
        let content = renderer.render(action.template, model, config.partials);
        fs.writeFileSync('./out/'+configName+'/'+action.output,content,'utf8');
    }
    fs.writeFileSync('./out/'+configName+'/LICENSE',fs.readFileSync('./templates/_common/LICENSE','utf8'),'utf8');
});

