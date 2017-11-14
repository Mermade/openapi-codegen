#!/usr/bin/env node
'use strict';

const fs = require('fs');

const yaml = require('js-yaml');
const mkdirp = require('mkdirp');

const adaptor = require('./adaptor.js');
const renderer = require('./index.js');

let s = fs.readFileSync('./specs/petstore3.json','utf8');
let o = yaml.safeLoad(s, { json: true } );

let model = adaptor.transform(o);

let templates = {};
templates.readme = fs.readFileSync('./templates/nodejs/README.mustache','utf8');
templates.swagger = fs.readFileSync('./templates/nodejs/swagger.mustache','utf8');
templates.package = fs.readFileSync('./templates/nodejs/package.mustache','utf8');
templates.controller = fs.readFileSync('./templates/nodejs/controller.mustache','utf8');

mkdirp('./out/nodejs');

let readme = renderer.render(templates.readme, model);
console.log(readme);
fs.writeFileSync('./out/nodejs/README.md',readme,'utf8');

let swagger = renderer.render(templates.swagger, model);
fs.writeFileSync('./out/nodejs/swagger.yaml',swagger,'utf8');

let packageJson = renderer.render(templates.package, model);
fs.writeFileSync('./out/nodejs/package.json',packageJson,'utf8');

let controller = renderer.render(templates.controller, model);
fs.writeFileSync('./out/nodejs/controller.js',controller,'utf8');
