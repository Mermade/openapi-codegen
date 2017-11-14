'use strict';

const yaml = require('js-yaml');
const deref = require('reftools/lib/dereference.js').dereference;

function transform(api) {
    let obj = {};

    obj["swagger-yaml"] = yaml.safeDump(api, {lineWidth:-1});
    obj["swagger-json"] = JSON.stringify(api, null, 2);
    
    obj.googleCloudFunctions = false;
    obj.implFolder = 'nodejs';
    
    obj.projectName = api.info.title;
    obj.appVersion = api.info.version;
    obj.appDescription = api.info.description||'No description';
    obj.classname = api.info.title.toLowerCase().split(' ').join('_').split('-').join('_');

    api = deref(api,api);

    obj.operations = [];
    for (let p in api.paths) {
        let pathItem = api.paths[p];
        for (let o in pathItem) {
            if (o === 'description') {
            }
            else if (o === 'summary') {
            }
            else if (o === 'parameters') {
            }
            else if (o === '$ref') {
            }
            else if (o.startsWith('x-')) {
            }
            else {
                let op = pathItem[o];
                let operation = {};
                operation.nickname = op.operationId;
                operation.allParams = [];
                for (let pa in op.parameters) {
                    let param = op.parameters[pa];
                    let parameter = {};
                    parameter.paramName = param.name;
                    parameter.baseName = param.name;
                    parameter.hasMore = (pa === op.parameters.length);
                    operation.allParams.push(parameter);
                }
                let container = {};
                container.classname = operation.nickname;
                container.operation = operation;
                container.allParams = operation.allParams; // dupe
                obj.operations.push(container);
            }
        }
    }

    return obj;
}

module.exports = {
    transform : transform
};

