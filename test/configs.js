'use strict';

const fs = require('fs');
const util = require('util');

const should = require('should');
const rf = require('node-readfiles');
const ajv = require('ajv')({
    allErrors: true,
    verbose: true,
    jsonPointers: true,
    patternGroups: true,
    extendRefs: true}); // optional, current default is to 'fail', spec behaviour is to 'ignore'

let schema = fs.readFileSync('./schemas/config.json');
let validator = ajv.compile(JSON.parse(schema));

let configs = {};
let templates = {};

function gatherConfigs(){
    return rf('./configs')
    .then(function(files){
        for (let file of files) {
            let s = fs.readFileSync('./configs/'+file,'utf8');
            let obj = JSON.parse(s);
            obj.name = file.replace('.json','');
            configs[obj.name] = obj;
        }
        return configs;
    });
}

function gatherTemplates(){
    return rf('./templates')
    .then(function(files){
        for (let file of files) {
            let components = file.split('\\').join('/').split('/');
            let config = components[0];
            components.shift();
            let name = components.join('/');
            if (!templates[config]) {
                templates[config] = [];
            }
            templates[config].push(name);
        }
        return templates;
    });
}

async function main(){
    await gatherConfigs();
    await gatherTemplates();

    describe('validate against schema',function(){
        Object.values(configs).forEach(function(config){
            it('should validate '+config.name,function(){
                validator(config);
                should(validator.errors).be.exactly(null);
            });
        });
    });
    
    describe('partials existence',function(){
        Object.values(configs).forEach(function(config){
            if (config.partials) {
                for (let p in config.partials) {
                    it('partial should exist '+config.name+'/'+p,function(){
                        fs.existsSync('./templates/'+config.name+'/'+config.partials[p]).should.be.exactly(true);
                    });
                }
            }
        });
    });
    
    describe('templates existence',function(){
        Object.values(configs).forEach(function(config){
            if (config.transformations) {
                for (let t of config.transformations) {
                    if (t.input) {
                       it('template should exist '+config.name+'/'+t.input,function(){
                            fs.existsSync('./templates/'+config.name+'/'+t.input).should.be.exactly(true);
                    });
                    }
                }
            }
        });
    });

    describe('check template dir for README etc',function(){
        Object.values(configs).forEach(function(config){
            let readme = false;
            if (config.transformations) {
                for (let tx of config.transformations) {
                    if (tx.output.toLowerCase().indexOf('readme')>=0) {
                        readme = true;
                    }
                }
                it('should have README '+(config.name||''),function(){
                    if (!readme) return this.skip('Boo');
                    readme.should.be.exactly(true);
                });
            }
        });
    });

    describe('templates accounted for',function(){
        Object.keys(templates).forEach(function(configName){
            let tmp = templates[configName];
            let config = configs[configName];
            if (config) {
                for (let name of tmp) {
                    it('template '+name+' should be accounted for in config '+configName,function(){
                        let found = 0;
                        if (config.partials) {
                            for (let p in config.partials) {
                            if (config.partials[p] === name) found++; 
                            }
                        }
                        for (let tx of config.transformations) {
                            if (name === tx.input) found++;
                        }
                        if (config.perModel) {
                            for (let pm of config.perModel) {
                                if (name === pm.input) found++;
                            }
                        }
                        if (config.perOperation) {
                            for (let po of config.perOperation) {
                                if (name === po.input) found++;
                            }
                        }
                        found.should.be.exactly(1);
                    });

                }
            }
        });
    });

    run();
}

main();
