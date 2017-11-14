'use strict';

const util = require('util');
const url = require('url');

const yaml = require('js-yaml');
const deref = require('reftools/lib/dereference.js').dereference;
const walkSchema = require('swagger2openapi/walkSchema').walkSchema;
const wsGetState = require('swagger2openapi/walkSchema').getDefaultState;
const validator = require('swagger2openapi/validate').validateSync;

function transform(api, defaults) {
    let obj = Object.assign({},defaults);

    obj["swagger-yaml"] = yaml.safeDump(api, {lineWidth:-1}); // set to original if converted v2.0
    obj["swagger-json"] = JSON.stringify(api, null, 2);
    obj["openapi-yaml"] = yaml.safeDump(api, {lineWidth:-1}); // set to original if converted v2.0
    obj["openapi-json"] = JSON.stringify(api, null, 2);
    
    obj.projectName = api.info.title;
    obj.appVersion = api.info.version;
    obj.apiVersion = api.info.version;
    obj.version = api.info.version;
    obj.swaggerVersion = api.openapi;
    obj.appDescription = api.info.description||'No description';
    obj.classname = api.info.title.toLowerCase().split(' ').join('_').split('-').join('_');
    obj.exportedName = obj.classname;
    obj.infoEmail = api.info.contact ? api.info.contact.email : null;
    obj.infoUrl = api.info.contact ? api.info.contact.url : null;
    obj.licenseInfo = api.info.license ? api.info.license.name : null;
    obj.licenseUrl = api.info.license ? api.info.license.url : null;
    obj.host = ''
    obj.basePath = '/';

    api = deref(api,api);

    obj.messages = [];
    let message = {};
    try {
        validator(api,{});
        message.level = 'Valid';
        message.elementType = 'Context';
        message.message = 'No errors detected';
        obj.messages.push(message);
    }
    catch (ex) {
        message.level = 'Error';
        message.elementType = 'Context';
        if (ex.options) {
            message.elementId = ex.options.context.pop();
        }
        message.message = ex.message;
        obj.messages.push(message);
    }

    if (api.servers && api.servers.length) {
        let u = api.servers[0].url;
        let up = url.parse(u);
        obj.host = up.host;
        obj.basePath = up.path;
    }

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
                operation.httpMethod = o;
                operation.path = p;
                operation.operationId = op.operationId;
                operation.allParams = [];
                operation.summary = op.summary;
                operation.notes = op.description;
                for (let pa in op.parameters) {
                    let param = op.parameters[pa];
                    let parameter = {};
                    parameter.paramName = param.name;
                    parameter.baseName = param.name;
                    parameter.dataType = param.schema.type;
                    parameter.description = param.description;
                    parameter.required = param.required;
                    parameter.hasMore = (pa != op.parameters.length-1);
                    operation.allParams.push(parameter);
                }
                let container = {};
                container.classname = operation.nickname;
                container.operation = operation;
                obj.operations.push(container);
            }
        }
    }

    obj.apiInfo = {};
    obj.apiInfo.apis = [];
    obj.apiInfo.apis.push( { operations: obj.operations } );

    obj.debugOperations = JSON.stringify(obj,null,2);

    obj.models = [];
    if (api.components) {
        for (let s in api.components.schemas) {
            let schema = api.components.schemas[s];
            let container = {}
            let model = {};
            model.name = s;
            model.title = schema.title;
            model.unescapedDescription = schema.description;
            model.vars = [];
            walkSchema(schema,{},wsGetState,function(schema,parent,state){
                let entry = {};
                entry.name = schema.name || schema.title;
                if (!entry.name && state.property && (state.property.startsWith('properties') || 
                    state.property.startsWith('additionalProperties'))) {
                    entry.name = state.property.split('/')[1];
                }
                entry.type = schema.type;
                entry.required = (parent.required && parent.required.indexOf(entry.name)>=0);
                entry.isPrimitiveType = ((schema.type !== 'object') && (schema.type !== 'array'));
                entry.complexType = schema.type;
                entry.dataFormat = schema.format;
                if (entry.name) model.vars.push(entry);
            });
            container.model = model;
            obj.models.push(container);
        }
    }

    obj.debugModels = JSON.stringify(obj.models,null,2);

    return obj;
}

module.exports = {
    transform : transform
};

