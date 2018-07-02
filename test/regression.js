'use strict';

const processor = require('../index.js');
const yaml = require('js-yaml');
const dircompare = require('dir-compare');
const should = require('should');
const JsDiff = require('diff');
const fs = require('fs');
const {promisify} = require('util');
const rimraf = require('rimraf');

const readFile = promisify(fs.readFile)

const {gatherConfigs, gatherTemplates} = require('./lib/loaders');

var format = require('util').format;

function stripDynamic(input) {
    input = input.replace(/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z/g, '--DATETIME--')
    input = input.replace(/([A-Z][a-z]{2} ){2}[0-9]{2} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}/g, '--DATETIME--')
    input = input.replace(/[a-f0-9]{8}-([a-f0-9]{4}-){3}[a-f0-9]{12}/, '--GUID--')
    return input
}

async function compare(fileA, fileB) {
    let res = await dircompare.compare(fileA, fileB, {
        compareContent: true
    })

    let files = res.diffSet.filter((e) => e.state !== 'equal')
    for (const e of files) {
        let files = await Promise.all([
            e.path1 + '/' + e.name1,
            e.path2 + '/' + e.name2
        ].map((f) => readFile(f, 'utf-8')));

        let diff = JsDiff.diffLines(stripDynamic(files[0]), stripDynamic(files[1]))

        diff.forEach(function(part){
            if(part.added) {
               throw new Error("added:   " + part.value);
            }
            if(part.removed) {
                throw new Error("removed: " + part.value);
            }
        });
    }
}
async function main(){
    let configs = await gatherConfigs();

    let s = fs.readFileSync('./defs/petstore3.json','utf8');
    let o = yaml.safeLoad(s, { json: true });

    describe('validate snapshots', function() {

        Object.keys(configs).forEach((k) => {
            let config = configs[k];
            let configName = k;
            config.outputDir = './tmp';
            config.templateDir = './templates/' + k;

            it('should build ' + k, function(done) {
                function cleanup () {
                    rimraf('./tmp/' + k, done)
                }
                processor.main(o,config,configName,function(){
                    compare('./snapshot/' + k, './tmp/' + k)
                    .then(cleanup, cleanup)
                });
            })
        })
    });

    run();

}
main();
