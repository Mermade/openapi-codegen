#!/usr/bin/env node
'use strict';

const fs = require('fs');

const yaml = require('js-yaml');
const mkdirp = require('mkdirp');

const adaptor = require('./adaptor.js');
const renderer = require('./index.js');

let s = fs.readFileSync('./specs/petstore3.json','utf8');
let o = yaml.safeLoad(s, { json: true } );

let configName = process.argv[2] || 'nodejs';
let config = require('./configs/'+configName+'.json');

let model = adaptor.transform(o, config.defaults);

let actions = [];
for (let t in config.transformations) {
    let tx = config.transformations[t];
    tx.template = fs.readFileSync('./templates/'+configName+'/'+tx.input,'utf8');
    actions.push(tx);
}

mkdirp('./out/'+configName);

for (let action of actions) {
    let content = renderer.render(action.template, model);
    fs.writeFileSync('./out/'+configName+'/'+action.output,content,'utf8');
}
