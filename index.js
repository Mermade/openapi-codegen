'use strict';

const fs = require('fs');
const util = require('util');

const mkdirp = require('mkdirp');
const rimraf = require('rimraf');
const mustache = require('mustache');

const adaptor = require('./adaptor.js');

function main(o, config, configName) {
    let verbose = config.defaults.verbose;
    let model = adaptor.transform(o, config.defaults);

    for (let p in config.partials) {
        let partial = config.partials[p];
        if (verbose) console.log('Processing partial '+partial);
        config.partials[p] = fs.readFileSync('./templates/'+configName+'/'+partial,'utf8');
    }

    let actions = [];
    for (let t in config.transformations) {
        let tx = config.transformations[t];
        if (tx.input) {
            if (verbose) console.log('Processing template '+tx.input);
            tx.template = fs.readFileSync('./templates/'+configName+'/'+tx.input,'utf8');
        }
        actions.push(tx);
    }

    if (verbose) console.log('Making and cleaning output directories');
    mkdirp('./out/'+configName,function(){
        rimraf('./out/'+configName+'/*',function(){
            if (config.directories) {
                for (let directory of config.directories) {
                    mkdirp.sync('./out/'+configName+'/'+directory);
                }
            }
            for (let action of actions) {
                if (verbose) console.log('Rendering '+action.output);
                let content = mustache.render(action.template, model, config.partials);
                fs.writeFileSync('./out/'+configName+'/'+action.output,content,'utf8');
            }
            if (config.apache) {
                fs.writeFileSync('./out/'+configName+'/LICENSE',fs.readFileSync('./templates/_common/LICENSE','utf8'),'utf8');
            }
            else {
                fs.writeFileSync('./out/'+configName+'/LICENSE',fs.readFileSync('./templates/_common/UNLICENSE','utf8'),'utf8');
            }
        });
    });
}

module.exports = {
    main : main
};

