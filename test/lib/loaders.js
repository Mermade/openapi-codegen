'use strict';
const rf = require('node-readfiles');
const fs = require('fs');

function gatherConfigs(){
    return rf('./configs')
    .then(function(files){
        let configs = {};
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
        let templates = {};
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

module.exports = {
	gatherConfigs,
	gatherTemplates
}
