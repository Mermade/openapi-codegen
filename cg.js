#!/usr/bin/env node
'use strict';

const fs = require('fs');
const url = require('url');
const util = require('util');

const yaml = require('js-yaml');
const fetch = require('node-fetch');
const co = require('co');
const swagger2openapi = require('swagger2openapi');
const stools = require('swagger-tools');

const processor = require('./index.js');

var argv = require('yargs')
    .usage('cg [options] {configName} {openapi-definition}')
    .boolean('debug')
    .alias('d','debug')
    .describe('debug','Turn on debugging information in the model')
    .boolean('lint')
    .alias('l','lint')
    .describe('lint','Lint input definition')
    .boolean('stools')
    .alias('s','stools')
    .describe('stools','Use swagger-tools to validate OpenAPI 2.0 definitions')
    .boolean('verbose')
    .describe('verbose','Increase verbosity')
    .alias('v','verbose')
    .version()
    .argv;

let configName = argv._[0] || 'nodejs';
let config = require('./configs/'+configName+'.json');
let defName = argv._[1] || './defs/petstore3.json';

function convert20(obj){
    if (argv.verbose) console.log('Converting OpenAPI 2.0 definition');
    swagger2openapi.convertObj(obj,{patch:true,warnOnly:true,direct:true},function(err,openapi){
        if (err) {
            console.error(util.inspect(err));
        }
        else {
            config.defaults.swagger = obj;
            processor.main(openapi,config,configName);
        }
    });
}

function convert12(api){
    if (argv.verbose) console.log('Converting Swagger 1.2 definition');
    let options = {};
    options.source = defName;
    var base = options.source.split('/');
    var filename = base.pop();
    var extension = '';
    if (filename.endsWith('.json')) {
        extension = '.json';
    }
    else if (filename.endsWith('yaml')) {
        extension = '.yaml';
    }
    else {
        base.push(filename);
    }
    base = base.join('/');

    //if (options.source.endsWith('.json') || options.source.endsWith('.yaml')) {
    //    extension = '';
    //}

    var retrieve = [];
    var apiDeclarations = [];
    if (api.apis) {
        for (var component of api.apis) {
            component.path = component.path.replace('.{format}','.json');
            var lbase = base;
            if (component.path.startsWith('/')) {
                let up = url.parse(base);
                lbase = up.protocol + '//' + up.host;
            }
            if ((base.endsWith('/')) && (component.path.startsWith('/'))) {
                lbase = base.substr(0,base.length-1);
            }
            if (component.path.indexOf('://')>=0) {
                lbase = '';
            }

            var u = (lbase+component.path);
            if (!u.endsWith(extension)) u += extension;
            if (argv.verbose) console.log(u);
            retrieve.push(fetch(u,options.fetchOptions)
            .then(res => {
                if (argv.verbose) console.log(res.status);
                return res.text();
            })
            .then(data => {
                apiDeclarations.push(yaml.safeLoad(data,{json:true}));
            })
            .catch(err => {
                console.error(util.inspect(err));
            }));
        }
    }

    co(function* () {
      // resolve multiple promises in parallel
      var res = yield retrieve;
      var sVersion = 'v1_2';
      stools.specs[sVersion].convert(api,apiDeclarations,true,function(err,converted){
          if (err) {
              console.error(util.inspect(err));
          }
          else {
              if (converted.info && !converted.info.version) {
                  converted.info.version = '1.0.0';
              }
              convert20(converted);
          }
      });
    });
}

function main(s) {
    let o = yaml.safeLoad(s, { json: true } );
    if (argv.verbose) console.log('Loaded definition '+defName);

    if (o && o.openapi) {
        processor.main(o,config,configName);
    }
    else {
        if (o && o.swaggerVersion && o.swaggerVersion === '1.2') {
            convert12(o);
        }
        else if (o && o.swagger && o.swagger === '2.0') {
            convert20(o);
        }
        else {
            console.error('Unrecognised OpenAPI/Swagger version');
        }
    }
}

if (argv.verbose) {
    config.defaults.verbose = true;
    console.log('Loaded config '+configName);
}
if (argv.lint) config.defaults.lint = true;
if (argv.debug) config.defaults.debug = true;
if (argv.stools) config.defaults.stools = true;

let up = url.parse(defName);
if (up.protocol && up.protocol.startsWith('http')) {
    fetch(defName)
    .then(function (res) {
        return res.text();
    }).then(function (body) {
        main(body);
    }).catch(function (err) {
        console.error(err.message);
    });
}
else {
   let s = fs.readFileSync(defName,'utf8');
   main(s);
}
