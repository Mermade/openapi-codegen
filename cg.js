#!/usr/bin/env node
// @ts-check
'use strict';

const fs = require('fs');
const path = require('path');
const url = require('url');
const util = require('util');

const yaml = require('yaml');
const fetch = require('node-fetch');
const proxyAgent = require('proxy-agent');
const co = require('co');
const swagger2openapi = require('swagger2openapi');
const stools = require('swagger-tools');
const mkdirp = require('mkdirp');
const rimraf = require('rimraf');
const admzip = require('adm-zip');

const processor = require('./local.js');
const remote = require('./remote.js');

async function list(provider, filter) {
    process.exitCode = await remote.list(provider, filter);
    process.exit();
}

var argv = require('yargs')
    .usage('cg [options] {[path]configName} {openapi-definition}')
    .boolean('debug')
    .alias('d','debug')
    .describe('debug','Turn on debugging information in the model')
    .string('filter')
    .describe('filter','Filter term to use with --list')
    .boolean('flat')
    .alias('f','flat')
    .describe('flat','Do not include config-name in output directory structure')
    .boolean('lint')
    .alias('l','lint')
    .describe('lint','Lint input definition')
    .string('list')
    .describe('list','List available templates for provider (og or sc)')
    .string('output')
    .alias('o','output')
    .describe('output','Specify output directory')
    .default('output','./out/')
    .boolean('stools')
    .alias('s','stools')
    .describe('stools','Use swagger-tools to validate OpenAPI 2.0 definitions')
    .string('templates')
    .alias('t','templates')
    .describe('templates','Specify templates directory')
    .boolean('verbose')
    .describe('verbose','Increase verbosity')
    .alias('v','verbose')
    .boolean('zip')
    .alias('z','zip')
    .describe('zip','Create a .zip file instead of individual files')
    .version()
    .argv;

if (argv.list) {
    list(argv.list, argv.filter);
}

let configStr = argv._[0] || 'nodejs';
let configName = path.basename(configStr);
let remoteConfig = configName.indexOf(':')>-1;
let configPath = path.dirname(configStr);
if ((configPath === '.') && (!configStr.startsWith('.'))) {
  configPath = '';
}
if (!configPath) configPath = path.resolve(__dirname,'configs');
let configFile = path.join(configPath,configName);
if (path.extname(configFile)) {
  configName = configName.replace(path.extname(configFile),'');
}
else {
  configFile += '.json';
}
let config = remoteConfig ? { defaults: {} } : yaml.parse(fs.readFileSync(configFile,'utf8'), {prettyErrors: true});
let defName = argv._[1] || path.resolve(__dirname,'defs/petstore3.json');

let finish = remoteConfig ? finishRemote : finishLocal;

config.outputDir = argv.output;
config.templateDir = argv.templates;

if (config.generator) {
    let generator_path = path.resolve(configPath, config.generator);
    config.generator = require(generator_path);
}

let zipFiles = {};

function nop(arg, callback) { if (callback) callback(null,true); return true; }

function zipFile(filename,contents,encoding) {
    zipFiles[filename] = contents;
}

function finishLocal(err,result) {
    if (argv.zip) {
        // create archive
        var zip = new admzip();

        // add files directly
        for (let f in zipFiles) {
            zip.addFile(f, new Buffer(zipFiles[f]), 'Created with OpenAPI-CodeGen');
        }
        // write everything to disk
        zip.writeZip(path.join(config.outputDir,configName+'.zip'));
    }
}

function finishRemote(err,result) {
   configName = configName.split(':').pop();
   if (argv.verbose) console.log('Making/cleaning output directories');
   mkdirp(path.join(config.outputDir,configName),function(){
       rimraf(path.join(config.outputDir,configName)+'/*',function(){
           if (argv.zip) {
              fs.writeFileSync(path.join(config.outputDir,configName,configName+'.zip'),result);
           }
           else {
               const zip = new admzip(result);
               if (argv.verbose) {
                   console.log('Unzipping...');
                   const zipEntries = zip.getEntries(); // an array of ZipEntry records
                   zipEntries.forEach(function(zipEntry) {
                       console.log(zipEntry.entryName);
                   });
                }
                zip.extractAllTo(config.outputDir,true);
            }
        });
    });
}

function despatch(obj, config, configName, callback) {
    if (remoteConfig) {
        remote.main(obj, config, configName, callback);
    }
    else {
        processor.main(obj, config, configName, callback);
    }
}

function convert20(obj){
    if (argv.verbose) console.log('Converting OpenAPI 2.0 definition');
    swagger2openapi.convertObj(obj,{patch:true,anchors:true,warnOnly:true,direct:true},function(err,openapi){
        if (err) {
            console.error(util.inspect(err));
        }
        else {
            config.defaults.swagger = obj;
            despatch(openapi,config,configName,finish);
        }
    });
}

function convert12(api){
    if (argv.verbose) console.log('Converting Swagger 1.2 definition');
    let options = {};
    options.source = defName;
    var aBase = options.source.split('/');
    var filename = aBase.pop();
    var extension = '';
    if (filename.endsWith('.json')) {
        extension = '.json';
    }
    else if (filename.endsWith('yaml')) {
        extension = '.yaml';
    }
    else {
        aBase.push(filename);
    }
    let base = aBase.join('/');

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
                apiDeclarations.push(yaml.parse(data));
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
    let o = yaml.parse(s, { prettyErrors: true });
    if (argv.verbose) console.log('Loaded definition '+defName);

    if (o && o.openapi) {
        despatch(o,config,configName,finish);
    }
    else {
        if (o && o.swaggerVersion && o.swaggerVersion === '1.2') {
            convert12(o);
        }
        else if (o && o.swagger && o.swagger === '2.0') {
            if (remoteConfig) {
                despatch(o,config,configName,finish);
            }
            else {
                convert20(o);
            }
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
if (argv.flat) config.defaults.flat = true;
if (argv.stools) config.defaults.stools = true;
if (argv.zip) {
    processor.fileFunctions.createFile = zipFile;
    processor.fileFunctions.rimraf = nop;
    processor.fileFunctions.mkdirp = nop;
    processor.fileFunctions.mkdirp.sync = nop;
}
config.defaults.source = defName;

try {
    const url = new URL(defName);
    fetch(url, { agent: proxyAgent() })
        .then(res => res.text())
        .then(body => main(body))
        .catch(err => console.error(err.message));
} catch (err) {
    const content = fs.readFileSync(defName, 'utf8');
    main(content);
}
