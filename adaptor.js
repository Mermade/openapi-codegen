'use strict';

const util = require('util');
const url = require('url');

const yaml = require('js-yaml');
const safeJson = require('safe-json-stringify');
const deref = require('reftools/lib/dereference.js').dereference;
const walkSchema = require('swagger2openapi/walkSchema').walkSchema;
const wsGetState = require('swagger2openapi/walkSchema').getDefaultState;
const validator = require('swagger2openapi/validate').validateSync;

String.prototype.toCamelCase = function camelize() {
    return this.toLowerCase().replace(/[-_ \/\.](.)/g, function (match, group1) {
        return group1.toUpperCase();
    });
}

// TODO add html and possibly termcap renderers
const markdownPPs = {
    nop: function(markdown) {
        return markdown;
    }
};

const typeMaps = {
    nop: function(type,required,schema) {
        return type;
    },
    java: function(type,required,schema) {
        let result = type;
        if (!required) result += '?';
        return result;
    },
    javascript: function(type,required,schema) {
        let result = type;
        if (result === 'integer') result = 'number';
        return result;
    },
    typescript: function(type,required,schema) {
        let result = type;
        if (result === 'integer') result = 'number';
        if (result === 'array') {
            result = 'Array';
            if (schema.items && schema.items.type) {
                result += '<'+typeMap(schema.items.type,false,schema.items)+'>';
            }
        }
        return result;
    }
};

let typeMap = typeMaps.nop;
let markdownPP = markdownPPs.nop;

function transform(api, defaults) {
    let obj = Object.assign({},defaults);

    let lang = (defaults.language||'').toLowerCase();
    if (typeMaps[lang]) typeMap = typeMaps[lang];

    obj["swagger-yaml"] = yaml.safeDump(defaults.swagger || api, {lineWidth:-1}); // set to original if converted v2.0
    obj["swagger-json"] = JSON.stringify(defaults.swagger || api, null, 2); // set to original if converted 2.0
    obj["openapi-yaml"] = yaml.safeDump(api, {lineWidth:-1});
    obj["openapi-json"] = JSON.stringify(api, null, 2);
    
    obj.projectName = api.info.title;
    obj.appVersion = api.info.version;
    obj.apiVersion = api.info.version;
    obj.packageVersion = api.info.version;
    obj.version = api.info.version;
    obj.swaggerVersion = api.openapi;
    obj.appDescription = api.info.description||'No description';
    obj.classname = api.info.title.toLowerCase().split(' ').join('_').split('-').join('_');
    obj.classVarName = 'default'; //? possibly an array of these based on tags (a la widdershins)
    obj.exportedName = obj.classname;
    obj.infoEmail = api.info.contact ? api.info.contact.email : null;
    obj.infoUrl = api.info.contact ? api.info.contact.url : null;
    obj.licenseInfo = api.info.license ? api.info.license.name : null;
    obj.licenseUrl = api.info.license ? api.info.license.url : null;
    obj.appName = api.info.title;
    obj.host = ''
    obj.basePath = '/';
    obj.contextPath = '/';
    obj.packageName = 'IO.OpenAPI';
    obj.invokerPackage = 'IO.OpenAPI';
    obj.hasImport = true;
    obj.modelPackage = 'IO.OpenAPI';
    obj.package = 'IO.OpenAPI.Api';
    obj.clientPackage = 'IO.OpenAPI.Client';
    obj.importPath = 'IO.OpenAPI.Api.Default';
    obj.hasMore = true;
    obj.generatedDate = new Date().toString();
    obj.generatorClass = 'class '+defaults.configName;
    obj.imports = [ { "import": "IO.OpenAPI.Model.Default" } ];
    obj.name = obj.classname;
    obj.classFilename = obj.classname;
    obj.jsModuleName = obj.classname;
    obj.jsProjectName = obj.classname;
    if (defaults.swagger) {
        obj.swagger = defaults.swagger;
    }
    else {
        obj.swagger = {};
        obj.swagger.swagger = api.openapi;
        obj.swagger.info = api.info;
        obj.tags = api.tags;
        obj.externalDocs = api.externalDocs;
        obj.swagger.paths = api.paths;
        if (api.components && api.components.schemas) {
            obj.swagger.definitions = api.components.schemas;
        }
    }

    if (api.components && api.components.securitySchemes) {
        obj.hasAuthMethods = true;
        obj.authMethods = [];
        for (let s in api.components.securitySchemes) {
            let scheme = api.components.securitySchemes[s];
            let entry = {};
            entry.name = s;
            if (scheme.type === 'http') {
                entry.isBasic = true;
            }
            else if (scheme.type === 'oauth2') {
                entry.isOAuth = true;
                if (scheme.flows) {
                    let flow = Object.values(scheme.flows)[0];
                    entry.authorizationUrl = flow.authorizationUrl;
                    entry.tokenUrl = flow.tokenUrl;
                }
            }
            else if (scheme.type == 'apiKey') {
                entry.isApiKey = true;
                entry.keyParamName = scheme.name;
                entry.isKeyInQuery = (scheme.in === 'query');
                entry.isKeyInHeader = (scheme.in === 'header');
                entry.isKeyInCookie = (scheme.in === 'cookie'); // extension
            }
            else {
                // TODO OpenAPI 3 schemes
            }
            obj.authMethods.push(entry);
        }

    }

    api = deref(api,api,{$ref:'x-oldref'});

    obj.messages = [];
    let message = {};
    let vOptions = {lint:defaults.lint};
    try {
        validator(api,vOptions);
        message.level = 'Valid';
        message.elementType = 'Context';
        message.elementId = 'None';
        message.message = 'No validation errors detected';
        obj.messages.push(message);
        if (defaults.verbose) console.log(message);
    }
    catch (ex) {
        message.level = 'Error';
        message.elementType = 'Context';
        message.elementId = vOptions.context.pop();
        message.message = ex.message;
        obj.messages.push(message);
        console.error(message);
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
                operation.httpMethod = o; //o.toUpperCase();
                operation.path = p;
                operation.operationId = op.operationId;
                operation.allParams = [];
                operation.pathParams = [];
                operation.queryParams = [];
                operation.headerParams = [];
                operation.formParams = [];
                operation.summary = op.summary;
                operation.notes = op.description;
                operation.responseHeaders = [];
                operation.hasProduces = true;
                operation.hasMore = true; // last one gets reset to false
                operation.isResponseBinary = false; //TODO
                operation.baseName = 'Default';
                if (op.tags && op.tags.length) {
                    operation.baseName = op.tags[0];
                }
                operation.produces = [];
                for (let pa in op.parameters) {
                    operation.hasParams = true;
                    let param = op.parameters[pa];
                    let parameter = {};
                    parameter.paramName = param.name;
                    parameter.baseName = param.name;
                    parameter.required = param.required;
                    parameter.dataType = typeMap(param.schema.type,parameter.required,param.schema);
                    parameter.dataFormat = param.schema.format;
                    parameter.description = param.description;
                    parameter.unescapedDescription = param.description;
                    parameter.defaultValue = param.default;
                    parameter.hasMore = true; // last one gets reset below after sorting
                    operation.allParams.push(parameter);
                    if (param.in === 'path') {
                        parameter.isPathParam = true;
                        operation.pathParams.push(parameter);
                        operation.hasPathParams = true;
                    }
                    if (param.in === 'query') {
                        parameter.isQueryParam = true;
                        operation.queryParams.push(parameter);
                        operation.hasQueryParams = true;
                    }
                    if (param.in === 'header') {
                        parameter.isHeaderParam = true;
                        operation.headerParams.push(parameter);
                        operation.hasHeaderParams = true;
                    }
                    if (param.in === 'form') {
                        parameter.isFormParam = true;
                        operation.formParams.push(parameter);
                        operation.hasFormParams = true;
                    }
                }
                if (op.requestBody) {
                    operation.hasBodyParam = true;
                    operation.bodyParam = {};
                    operation.bodyParam.isBodyParam = true;
                    operation.bodyParam.baseName = 'body';
                    operation.bodyParam.paramName = 'body';
                    operation.bodyParam.dataType = 'object'; // can be changed below
                    operation.bodyParam.description = op.requestBody.description;
                    operation.bodyParam.schema = {};
                    operation.bodyParam.isEnum = false;
                    operation.bodyParam.vendorExtensions = {};
                    operation.bodyParam.required = op.requestBody.required;
                    if (op.requestBody.content) {
                        let contentType = Object.values(op.requestBody.content)[0];
                        operation.bodyParam.schema = contentType.schema;
                        if (contentType.schema.type) {
                            operation.bodyParam.type = contentType.schema.type;
                        }
                    }
                    operation.bodyParam.jsonSchema = safeJson({schema: operation.bodyParam.schema},null,2);
                    operation.bodyParams = [];
                    operation.bodyParams.push(operation.bodyParam);
                    operation.bodyParam.hasMore = true;
                    operation.allParams.push(operation.bodyParam);
                }
                operation.tags = op.tags;
                operation.imports = op.tags;
                operation.vendorExtensions = {};

                operation.responses = [];
                for (let r in op.responses) {
                    let response = op.responses[r];
                    let entry = {};
                    entry.code = r;
                    entry.nickname = 'response'+r;
                    entry.message = response.description;
                    entry.simpleType = true;
                    entry.schema = {};
                    entry.jsonSchema = safeJson({ schema: entry.schema },null,2);
                    if (response.content) {
                        entry.dataType = 'object';
                        let contentType = Object.values(response.content)[0];
                        if (contentType.schema) {
                            entry.schema = contentType.schema;
                            entry.jsonSchema = safeJson({schema:entry.schema},null,2);
                            entry.dataType = contentType.schema.type;
                            if (contentType.schema["x-oldref"]) {
                                entry.dataType = contentType.schema["x-oldref"].replace('#/components/schemas/','');
                            }
                        }
                    }
                    // TODO examples
                    operation.responses.push(entry);
                }

                operation.allParams = operation.allParams.sort(function(a,b){
                    if (a.required && !b.required) return -1;
                    if (b.required && !a.required) return +1;
                    return 0;
                });
                if (operation.allParams.length) {
                    operation.allParams[operation.allParams.length-1].hasMore = false;
                }

                let container = {};
                container.baseName = operation.nickname;
                container.classname = operation.nickname;
                container.operation = operation;
                obj.operations.push(container);
            }
        }
    }

    if (obj.operations) {
        obj.operations[obj.operations.length-1].operation.hasMore = false;
    }

    obj.apiInfo = {};
    obj.apiInfo.apis = [];
    obj.apiInfo.apis.push( { operations: obj.operations } );

    if (defaults.debug) obj.debugOperations = JSON.stringify(obj,null,2);

    obj.models = [];
    if (api.components) {
        for (let s in api.components.schemas) {
            let schema = api.components.schemas[s];
            let container = {}
            let model = {};
            model.name = s;
            model.classname = s;
            model.classVarName = s;
            model.modelJson = safeJson(schema,null,2);
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
                entry.getter = ('get_'+entry.name).toCamelCase();
                entry.setter = ('set_'+entry.name).toCamelCase();
                entry.type = schema.type;
                entry.required = (parent.required && parent.required.indexOf(entry.name)>=0);
                entry.type = typeMap(entry.type,entry.required,schema);
                entry.datatype = entry.type; //?
                entry.datatypeWithEnum = entry.datatype; // ?
                entry.jsonSchema = safeJson(schema,null,2);
                entry.hasMore = true;
                entry.isPrimitiveType = ((schema.type !== 'object') && (schema.type !== 'array'));
                entry.isNotContainer = entry.isPrimitiveType;
                if ((schema.type === 'object') && schema.properties && schema.properties["x-oldref"]) {
                    entry.complexType = schema.properties["x-oldref"].replace('#/components/schemas/','');
                }
                
                entry.dataFormat = schema.format;
                entry.defaultValue = schema.default;
                entry.isEnum = false;
                if (entry.name && state.depth<=1) {
                    entry.baseName = entry.name.toLowerCase();
                    model.vars.push(entry);
                }
            });
            container.model = model;
            obj.models.push(container);
        }
    }

    if (defaults.debug) obj.debugModels = JSON.stringify(obj.models,null,2);

    return obj;
}

module.exports = {
    transform : transform
};

