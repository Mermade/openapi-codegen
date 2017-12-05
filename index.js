'use strict';

const fs = require('fs');
const path = require('path');
const util = require('util');

const mkdirp = require('mkdirp');
const rimraf = require('rimraf');
const Hogan = require('hogan.js');
const clone = require('reftools/lib/clone.js').deepClone; // must preserve functions

const adaptor = require('./adaptor.js');

// allows other backends, such as a stream writer for .tar.gz files
let ff = {
    readFileSync: fs.readFileSync,
    createFile: fs.writeFileSync,
    rimraf: rimraf,
    mkdirp: mkdirp
};

function tpl(...segments) {
    return path.join(__dirname, 'templates', ...segments)
};

function main(o, config, configName, callback) {
    let outputDir = config.outputDir || './out/';
    let verbose = config.defaults.verbose;
    config.defaults.configName = configName;
    adaptor.transform(o, config.defaults, function(err, model) {
        for (let p in config.partials) {
            let partial = config.partials[p];
            if (verbose) console.log('Processing partial '+partial);
            config.partials[p] = ff.readFileSync(tpl(configName, partial),'utf8');
        }
    
        let actions = [];
        for (let t in config.transformations) {
            let tx = config.transformations[t];
            if (tx.input) {
                if (verbose) console.log('Processing template '+tx.input);
                tx.template = ff.readFileSync(tpl(configName, tx.input),'utf8');
            }
            actions.push(tx);
        }
    
        if (verbose) console.log('Making/cleaning output directories');
        ff.mkdirp(outputDir+configName,function(){
            ff.rimraf(outputDir+configName+'/*',function(){
                if (config.directories) {
                    for (let directory of config.directories) {
                        ff.mkdirp.sync(outputDir+configName+'/'+directory);
                    }
                }
                for (let action of actions) {
                    if (verbose) console.log('Rendering '+action.output);
                    let template = Hogan.compile(action.template);
                    let content = template.render(model,config.partials);
                    ff.createFile(outputDir+configName+'/'+action.output,content,'utf8');
                }
                if (config.touch) { // may not now be necessary
                    let touchTmp = Hogan.compile(config.touch);
                    let touchList = touchTmp.render(model,config.partials);
                    let files = touchList.split('\r').join('').split('\n');
                    for (let file of files) {
                        file = file.trim();
                        if (file) {
                            if (!fs.existsSync(outputDir+configName+'/'+file)) {
                                ff.createFile(outputDir+configName+'/'+file,'','utf8');
                            }
                        }
                    }
                }
                if (config.apache) {
                    ff.createFile(outputDir+configName+'/LICENSE',ff.readFileSync(tpl('_common', 'LICENSE'),'utf8'),'utf8');
                }
                else {
                    ff.createFile(outputDir+configName+'/LICENSE',ff.readFileSync(tpl('_common', 'UNLICENSE'),'utf8'),'utf8');
                }
                let outer = model;
     
                if (config.perApi) {
                    let toplevel = clone(model);
                    delete toplevel.apiInfo;
                    for (let pa of config.perApi) {
                        let fnTemplate = Hogan.compile(pa.output);
                        let template = Hogan.compile(ff.readFileSync('./templates/'+configName+'/'+pa.input,'utf8'));
                        for (let api of model.apiInfo.apis) {
                            let cApi = Object.assign({},config.defaults,toplevel,api);
                            let filename = fnTemplate.render(cApi,config.partials);
                            if (verbose) console.log('Rendering '+filename+' (dynamic)');
                            ff.createFile(outputDir+configName+'/'+filename,template.render(cApi,config.partials),'utf8');
                        }
                    }
                }

                if (config.perModel) {
                    let cModels = clone(model.models); 
                    for (let pm of config.perModel) {
                        let fnTemplate = Hogan.compile(pm.output);
                        let template = Hogan.compile(ff.readFileSync('./templates/'+configName+'/'+pm.input,'utf8'));
                        for (let model of cModels) {
                            outer.models = [];
                            outer.models.push(model);
                            let filename = fnTemplate.render(outer,config.partials);
                            if (verbose) console.log('Rendering '+filename+' (dynamic)');
                            ff.createFile(outputDir+configName+'/'+filename,template.render(outer,config.partials),'utf8');
                        }
                    }
                }
    
                if (config.perOperation) { // now may not be necessary
                    for (let po of config.perOperation) {
                        for (let api of outer.apiInfo.apis) {
                            let cOperations = clone(api.operations);
                            let fnTemplate = Hogan.compile(po.output);
                            let template = Hogan.compile(ff.readFileSync('./templates/'+configName+'/'+po.input,'utf8'));
                            for (let operation of cOperations) {
                                model.operations = [];
                                model.operations.push(operation);
                                let filename = fnTemplate.render(outer,config.partials);
                                if (verbose) console.log('Rendering '+filename+' (dynamic)');
                                ff.createFile(outputDir+configName+'/'+filename,template.render(outer,config.partials),'utf8');
                            }
                        }
                    }
                }

                if (callback) callback(null,true);
            });
        });
    });
}

module.exports = {
    fileFunctions: ff,
    main : main
};

