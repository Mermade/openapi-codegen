// @ts-check
'use strict';

const util = require('util');
const url = require('url');

const yaml = require('yaml');
const uuidv4 = require('uuid').v4;
const safeJson = require('fast-safe-stringify');
const Case = require('case');
const stools = require('swagger-tools');
const sampler = require('openapi-sampler');
const deref = require('reftools/lib/dereference.js').dereference;
const clone = require('reftools/lib/clone.js').circularClone;
const walkSchema = require('oas-schema-walker').walkSchema;
const wsGetState = require('oas-schema-walker').getDefaultState;
const validator = require('oas-validator').validateInner;
const downconverter = require('./lib/orange/downconvert.js');

const schemaProperties = [
    'format',
    'minimum',
    'maximum',
    'exclusiveMinimum',
    'exclusiveMaximum',
    'minLength',
    'maxLength',
    'multipleOf',
    'minItems',
    'maxItems',
    'uniqueItems',
    'minProperties',
    'maxProperties',
    'additionalProperties',
    'pattern',
    'enum',
    'default'
];

// used by helper functions / convertToArray's toString method
let arrayMode = 'length';
let thisFunc = encodeURIComponent;

function safeSample(schema,options,api) {
    try {
        return sampler.sample(schema,options,api);
    }
    catch (ex) {
        console.warn('Sampler:',ex.message);
    }
    return {};
}

function convertArray(arr) {
    if (!arr) arr = [];
    if (arr.length) {
        arr.isEmpty = false;
        for (let i=0;i<arr.length;i++) {
            arr[i]['-first'] = (i === 0);
            arr[i]['-last'] = (i === arr.length-1);
            arr[i].hasMore = (i<arr.length-1);
        }
    }
    else arr.isEmpty = true;
    arr.toString = function() { if (arrayMode === 'length') return this.length.toString() };
    return arr;
}

function getAuthData(secSchemes,api) {
    let result = {};
    result.hasAuthMethods = (secSchemes && secSchemes.length>0);
    result.authMethods = [];
    if (result.hasAuthMethods) {
    for (let ss of secSchemes) {
        for (let s in ss) {
        let scheme = api.components.securitySchemes[s];
        let entry = {};
        entry.name = s;
        entry.isApiKey = false;
        entry.isBasic = false;
        entry.isOAuth = false;
        if (scheme.type === 'http') {
            entry.isBasic = true;
        }
        else if (scheme.type === 'oauth2') {
            entry.isOAuth = true;
            if (scheme.flows) {
                entry.flow = Object.keys(scheme.flows)[0];
                let flow = Object.values(scheme.flows)[0];
                entry.authorizationUrl = flow.authorizationUrl;
                entry.tokenUrl = flow.tokenUrl;
                entry.scopes = [];
                if (flow.scopes) {
                    for (let scope in flow.scopes) {
                        let sc = {};
                        sc.scope = scope;
                        entry.scopes.push(sc);
                    }
                }
                // override scopes with local subset
                if (Array.isArray(ss[s])) {
                    let newScopes = [];
                    for (let scope of entry.scopes) {
                        if (ss[s].indexOf(scope.scope)>=0) {
                            newScopes.push(scope);
                        }
                    }
                    entry.scopes = newScopes;
                }
                entry.scopes = convertArray(entry.scopes);
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
            entry.openapi = {};
            entry.openapi.scheme = scheme;
        }
        result.authMethods.push(entry);
    }
    }
    result.authMethods = convertArray(result.authMethods);
    }
    return result;
}

function specificationExtensions(obj) {
    let result = {};
    for (let k in obj) {
       if (k.startsWith('x-')) result[k] = obj[k];
    }
    return result;
}

function convertOperation(op,verb,path,pathItem,obj,api) {
    let operation = {};
    operation.httpMethod = verb.toUpperCase();
    if (obj.httpMethodCase === 'original') operation.httpMethod = verb; // extension
    operation.path = path;
    operation.replacedPathName = path; //?

    operation.description = op.description;
    operation.summary = op.summary;
    operation.allParams = [];
    operation.pathParams = [];
    operation.queryParams = [];
    operation.headerParams = [];
    operation.formParams = [];
    operation.summary = op.summary;
    operation.notes = op.description;
    if (!operation.notes) {
        operation.notes = {isEmpty:true};
        operation.notes.toString = function() { return '' };
    }
    //operation.hasMore = true; // last one gets reset to false
    operation.isResponseBinary = false; //TODO
    operation.isResponseFile = false; //TODO
    operation.baseName = 'Default';
    if (op.tags && op.tags.length) {
        operation.baseName = op.tags[0];
    }
    operation.produces = [];
    operation.consumes = [];
    operation.hasParams = false;
    operation.hasOptionalParams = false;
    operation.hasRequiredParams = false;
    operation.hasQueryParams = false;
    operation.hasFormParams = false;
    operation.hasPathParams = false;
    operation.hasHeaderParams = false;
    operation.hasBodyParam = false;
    operation.openapi = {};

    let authData = getAuthData(op.security||api.security,api);
    operation = Object.assign(operation,authData);

    let effParameters = (op.parameters||[]).concat(pathItem.parameters||[]);
    effParameters = effParameters.filter((param, index, self) => self.findIndex((p) => {return p.name === param.name && p.in === param.in; }) === index);

    const paramList = [];
    for (let pa in effParameters) {
        operation.hasParams = true;
        let param = effParameters[pa];
        let parameter = {};
        parameter.isHeaderParam = false;
        parameter.isQueryParam = false;
        parameter.isPathParam = false;
        parameter.isBodyParam = false;
        parameter.isFormParam = false;
        parameter.paramName = param.name;
        parameter.baseName = param.name;
        paramList.push(param.name);
        parameter.required = param.required||false;
        parameter.optional = !parameter.required;
        if (parameter.required) operation.hasRequiredParams = true;
        if (!parameter.required) operation.hasOptionalParams = true;
        parameter.dataType = typeMap(param.schema.type,parameter.required,param.schema);
        parameter["%dataType%"] = parameter.dataType; // bug in typescript-fetch template? trying to use {{{ with different delimiters
        for (let p of schemaProperties) {
            if (typeof param.schema[p] !== 'undefined') parameter[p] = param.schema[p];
        }
        parameter.example = JSON.stringify(safeSample(param.schema,{quiet:true},api));
        parameter.isBoolean = (param.schema.type === 'boolean');
        parameter.isPrimitiveType = (!param.schema["x-oldref"]);
        parameter.dataFormat = param.schema.format;
        parameter.isDate = (parameter.dataFormat == 'date');
        parameter.isDateTime = (parameter.dataFormat == 'date-time');
        parameter.description = param.description||'';
        parameter.unescapedDescription = param.description;
        parameter.defaultValue = (param.schema && typeof param.schema.default !== 'undefined') ? param.schema.default : undefined;
        parameter.isFile = false;
        parameter.isEnum = false; // TODO?
        parameter.vendorExtensions = specificationExtensions(param);
        if (param.schema && param.schema.nullable) {
            parameter.vendorExtensions["x-nullable"] = true;
        }
        if (param.style === 'form') {
            if (param.explode) {
                parameter.collectionFormat = 'multi';
            }
            else {
                parameter.collectionFormat = 'csv';
            }
        }
        else if (param.style === 'simple') {
            parameter.collectionFormat = 'csv';
        }
        else if (param.style === 'spaceDelimited') {
            parameter.collectionFormat = 'ssv';
        }
        else if (param.style === 'pipeDelimited') {
            parameter.collectionFormat = 'pipes';
        }
        if ((param["x-collectionFormat"] === 'tsv') || (param["x-tabDelimited"])) {
            parameter.collectionFormat = 'tsv';
        }

        operation.allParams.push(parameter);
        if (param.in === 'path') {
            parameter.isPathParam = true;
            operation.pathParams.push(clone(parameter));
            operation.hasPathParams = true;
        }
        if (param.in === 'query') {
            parameter.isQueryParam = true;
            operation.queryParams.push(clone(parameter));
            operation.hasQueryParams = true;
        }
        if (param.in === 'header') {
            parameter.isHeaderParam = true;
            operation.headerParams.push(clone(parameter));
            operation.hasHeaderParams = true;
        }
        /* if (param.in === 'form') { // TODO need to do this in requestBody
            parameter.isFormParam = true;
            operation.formParams.push(clone(parameter));
            operation.hasFormParams = true;
        }*/
    } // end of effective parameters

    operation.operationId = op.operationId || Case.camel((op.tags ? op.tags[0] :  '') + (paramList ? '_' + paramList.join('_') + '_' : '') + verb);
    operation.operationIdLowerCase = operation.operationId.toLowerCase();
    operation.operationIdSnakeCase = Case.snake(operation.operationId);
    operation.nickname = operation.operationId;

    operation.bodyParams = [];
    if (op.requestBody) {
        operation.openapi.requestBody = op.requestBody;
        operation.hasParams = true;
        operation.hasBodyParam = true;
        operation.bodyParam = {};
        operation.bodyParam.isBodyParam = true;
        operation.bodyParam.isHeaderParam = false;
        operation.bodyParam.isQueryParam = false;
        operation.bodyParam.isPathParam = false;
        operation.bodyParam.isFormParam = false;
        operation.bodyParam.isDate = false;
        operation.bodyParam.isDateTime = false;
        operation.bodyParam.baseName = 'body';
        operation.bodyParam.paramName = 'body';
        operation.bodyParam.baseType = 'object';
        operation.bodyParam.required = op.requestBody.required||false;
        operation.bodyParam.optional = !operation.bodyParam.required;
        if (operation.bodyParam.required) operation.hasRequiredParams = true;
        if (!operation.bodyParam.required) operation.hasOptionalParams = true;
        operation.bodyParam.dataType = typeMap('object',operation.bodyParam.required,{}); // can be changed below
        operation.bodyParam.description = op.requestBody.description||'';
        operation.bodyParam.schema = {};
        operation.bodyParam.isEnum = false; // TODO?
        operation.bodyParam.vendorExtensions = specificationExtensions(op.requestBody);
        if (op.requestBody.content) {
            let contentType = Object.values(op.requestBody.content)[0];
            let mt = { mediaType: Object.keys(op.requestBody.content)[0] };
            operation.consumes.push(mt);
            operation.hasConsumes = true;
            let tmp = obj.consumes.find(function(e,i,a){
                return (e.mediaType === mt.mediaType);
            });
            if (!tmp) {
                obj.consumes.push(clone(mt)); // so convertArray works correctly
                obj.hasConsumes = true;
            }
            operation.bodyParam.schema = contentType.schema;
            operation.bodyParam.example = JSON.stringify(safeSample(contentType.schema,{quiet:true},api));
            for (let p in schemaProperties) {
                if (typeof contentType.schema[p] !== 'undefined') operation.bodyParam[p] = contentType.schema[p];
            }
            if (contentType.schema.type) {
                operation.bodyParam.type = contentType.schema.type;
                operation.bodyParam.dataType = typeMap(contentType.schema.type,operation.bodyParam.required,contentType.schema); // this is the below mentioned
            }
        }
        operation.bodyParam["%dataType%"] = operation.bodyParam.dataType; // bug in typescript-fetch template?
        operation.bodyParam.jsonSchema = safeJson({schema: operation.bodyParam.schema},null,2);
        operation.bodyParams.push(operation.bodyParam);
        operation.bodyParam.isFile = false; // TODO
        operation.allParams.push(clone(operation.bodyParam));
    }
    operation.tags = op.tags;
    operation.imports = op.tags;
    operation.vendorExtensions = specificationExtensions(op);

    operation.responses = [];
    for (let r in op.responses) {
        if (!r.startsWith('x-')) {
        let response = op.responses[r];
        let entry = {};
        entry.code = r;
        entry.isDefault = (r === 'default');
        entry.nickname = 'response'+r;
        entry.message = response.description;
        entry.description = response.description||'';
        entry.simpleType = true;
        entry.schema = {};
        entry.jsonSchema = safeJson({ schema: entry.schema },null,2);
        if (response.content) {
            entry.baseType = 'object';
            entry.dataType = typeMap(entry.baseType,false,{});
            let contentType = Object.values(response.content)[0];
            let mt = {};
            mt.mediaType = Object.keys(response.content)[0];
            operation.produces.push(mt);
            operation.hasProduces = true;
            let tmp = obj.produces.find(function(e,i,a){
                return (e.mediaType === mt.mediaType);
            });
            if (!tmp) {
                obj.produces.push(clone(mt)); // so convertArray works correctly
                obj.hasProduces = true;
            }
            if (contentType && contentType.schema) {
                entry.schema = contentType.schema;
                entry.jsonSchema = safeJson({schema:entry.schema},null,2);
                entry.baseType = contentType.schema.type;
                entry.isPrimitiveType = true;
                entry.dataType = typeMap(contentType.schema.type,false,entry.schema);
                if (contentType.schema["x-oldref"]) {
                    entry.dataType = contentType.schema["x-oldref"].replace('#/components/schemas/','');
                    entry.isPrimitiveType = false;
                }
            }
            if (contentType && contentType.example) {
                entry.hasExamples = true;
                if (!entry.examples) entry.examples = [];
                entry.examples.push({contentType: mt.mediaType, example: JSON.stringify(contentType.example,null,2)});
            }
            if (contentType && contentType.examples) {
                for (let ex in contentType.examples) {
                    const example = contentType.examples[ex];
                    if (example.value) {
                        entry.hasExamples = true;
                        if (!entry.examples) entry.examples = [];
                        entry.examples.push({contentType: mt.mediaType, example: JSON.stringify(example.value,null,2)});
                    }
                }
            }

            if (!entry.hasExamples && entry.schema) {
                let example = safeSample(entry.schema,{quiet:true},api);
                if (example) {
                    entry.hasExamples = true;
                    if (!entry.examples) entry.examples = [];
                    entry.examples.push({contentType: mt.mediaType, example:JSON.stringify(example,null,2)});
                }
            }

            operation.examples = (operation.examples||[]).concat(entry.examples||[]);

            operation.returnType = entry.dataType;
            operation.returnBaseType = entry.baseType;
            operation.returnTypeIsPrimitive = entry.isPrimitiveType;
            operation.returnContainer = ((entry.baseType === 'object') || (entry.baseType === 'array'));

        }
        entry.responseHeaders = []; // TODO responseHeaders
        entry.responseHeaders = convertArray(entry.responseHeaders);
        entry.examples = convertArray(entry.examples);
        entry.openapi = {};
        entry.openapi.links = response.links;
        operation.responses.push(entry);
        operation.responses = convertArray(operation.responses);
    }

    if (obj.sortParamsByRequiredFlag) {
        operation.allParams = operation.allParams.sort(function(a,b){
            if (a.required && !b.required) return -1;
            if (b.required && !a.required) return +1;
            return 0;
        });
    }
    }
    operation.queryParams = convertArray(operation.queryParams);
    operation.headerParams = convertArray(operation.headerParams);
    operation.pathParams = convertArray(operation.pathParams);
    operation.formParams = convertArray(operation.formParams);
    operation.bodyParams = convertArray(operation.bodyParams);
    operation.allParams = convertArray(operation.allParams);
    operation.examples = convertArray(operation.examples);

    if (operation.hasConsumes) {
        operation.consumes = convertArray(operation.consumes);
    }
    else {
        delete operation.consumes;
    }
    if (operation.hasProduces) {
        operation.produces = convertArray(operation.produces);
    }
    else {
        delete operation.produces;
    }

    operation.openapi.callbacks = op.callbacks;

    //let container = {};
    //container.baseName = operation.nickname;
    //container.operation = operation;
    //obj.operations.push(container);
    return operation;
}

function convertToApis(source,obj,defaults) {
    let apis = [];
    for (let p in source.paths) {
        for (let m in source.paths[p]) {
            if ((m !== 'parameters') && (m !== 'summary') && (m !== 'description') && (!m.startsWith('x-'))) {
                let op = source.paths[p][m];
                let tagName = 'Default';
                if (op.tags && op.tags.length > 0) {
                    tagName = op.tags[0];
                }
                let entry = apis.find(function(e,i,a){
                    return (e.name === tagName);
                });
                if (!entry) {
                    entry = {};
                    entry.name = tagName;
                    //if (defaults.language === 'typescript') {
                    //    entry.classname = Case.pascal(entry.name);
                    //}
                    //else {
                    entry.classname = tagName+'Api';
                    //}
                    entry.classFilename = tagName+'Api';
                    entry.classVarName = tagName; // see issue #21
                    entry.packageName = obj.packageName; //! this may not be enough / sustainable. Or many props at wrong level :(
                    entry.operations = {};
                    entry.operations.operation = [];
                    apis.push(entry);
                }
                let operation = convertOperation(op,m,p,source.paths[p],obj,source);
                entry.operations.operation.push(operation);
            }
        }
    }
    for (let t in source.tags) {
        let tag = source.tags[t];
        let entry = apis.find(function(e,i,a){
            return (e.name === t);
        });
        if (entry) {
            entry.classname = tag.name+'Api';
            entry.description = tag.description;
            entry.externalDocs = tag.externalDocs;
        }
    }
    for (let api of apis) {
        api.operations.operation = convertArray(api.operations.operation);
    }
    apis = convertArray(apis);
    return apis;
}

function convertToPaths(source,obj,defaults) {
    let paths = [];
    for (let p in source.paths) {
        for (let m in source.paths[p]) {
            if ((m !== 'parameters') && (m !== 'summary') && (m !== 'description') && (!m.startsWith('x-'))) {
                let op = source.paths[p][m];
                let tagName = 'Default';
                if (op.tags && op.tags.length > 0) {
                    tagName = op.tags[0];
                }
                let entry = paths.find(function(e,i,a){
                    return (e.name === p);
                });
                if (!entry) {
                    const split = p.replace(/^\//,'').split(/\//g);
                    const dirname = split.slice(0,-1).join('/');
                    const filename = split.slice(-1).join('');
                    // Generates class name from path using the rules
                    // * the slashes are stripped out
                    // * each path part is capitalised
                    // * each parameter is changed to By<param>
                    // i.e.
                    // /users => Users
                    // /users/{id} => UsersById
                    // /users/{id}/delete => UsersByIdDelete
                    const className = split.map(v=>v.replace(/{([^}]+)}/g,(v,v1)=>`By${v1[0].toUpperCase()}${v1.slice(1)}`).replace(/^./,(v)=>`${v[0].toUpperCase()}${v.slice(1)}`)).join('');

                    entry = {};
                    entry.name = p;
                    //if (defaults.language === 'typescript') {
                    //    entry.classname = Case.pascal(entry.name);
                    //}
                    //else {
                    entry.classname = className+'Api';
                    //}
                    entry.classDirName = dirname;
                    entry.classFilename = filename;
                    entry.classVarName = tagName; // see issue #21
                    entry.packageName = obj.packageName; //! this may not be enough / sustainable. Or many props at wrong level :(
                    entry.operations = {};
                    entry.operations.operation = [];
                    paths.push(entry);
                }
                let operation = convertOperation(op,m,p,source.paths[p],obj,source);
                entry.operations.operation.push(operation);
            }
        }
    }
    for (let t in source.tags) {
        let tag = source.tags[t];
        let entry = paths.find(function(e,i,a){
            return (e.name === t);
        });
        if (entry) {
            entry.classname = tag.name+'Api';
            entry.description = tag.description;
            entry.externalDocs = tag.externalDocs;
        }
    }
    for (let path of paths) {
        path.operations.operation = convertArray(path.operations.operation);
    }
    paths = convertArray(paths);
    return paths;
}

// TODO add html and possibly termcap (https://www.npmjs.com/package/hermit) renderers
const markdownPPs = {
    nop: function(markdown) {
        return markdown;
    }
};

function getSchemaType(schema) {
    if (!schema.type && schema["x-oldref"]) {
        return schema["x-oldref"].replace('#/components/schemas/', '');
    }

    return schema.type;
}

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
            if (schema.items) {
                result += '<'+typeMap(getSchemaType(schema.items), false, schema.items)+'>';
            }
        }
        return result;
    },
    go: function(type,required,schema) {
        let result = type;
        if (result === 'integer') result = 'int';
        if (result === 'boolean') result = 'bool';
        if (result === 'object') result = 'struct{}';
        if (result === 'array') {
            result = '[100]'; //!
            if (schema.items && schema.items.type) {
                result += typeMap(schema.items.type,false,schema.items);
            }
        }
        return result;
    }
};

const reservedWords = {
    nop: [],
    go: [ 'type' ]
};

let typeMap = typeMaps.nop;
let markdownPP = markdownPPs.nop;
let reserved = reservedWords.nop;

function getBase() {
    let base = {};
    base.supportingFiles = [];
    base.modelTests = [];
    base.modelDocs = [];
    base.apiTests = [];
    base.apiDocs = [];
    base.allowUnicodeIdentifiers = false; /* boolean, toggles whether unicode identifiers are allowed in names or not, default is false */
    //base.developerName = x; /* developer name in generated pom.xml */
    //base.developerEmail = x; /* developer email in generated pom.xml */
    //base.developerOrganization = x; /* developer organization in generated pom.xml */
    //base.developerOrganizationUrl = x; /* developer organization URL in generated pom.xml */
    base.gitUserId = 'Mermade'; /* Git user ID, e.g. swagger-api. */
    base.gitRepoId = 'openapi-codegen'; /* Git repo ID, e.g. swagger-codegen. */
    base.licenseName = 'Unlicense'; /* The name of the license */
    base.projectLicenseName = 'Unlicense'; /* The name of the license */
    base.licenseUrl = 'https://unlicense.org'; /* The URL of the license */
    base.projectLicenseUrl = 'https://unlicense.org'; /* The URL of the license */
    base.projectUrl = 'https://github.com/Mermade/openapi-codegen';
    base.localVariablePrefix = ''; /* prefix for generated code members and local variables */
    base.serializableModel = true; /* boolean - toggle "implements Serializable" for generated models */
    base.bigDecimalAsString = false; /* Treat BigDecimal values as Strings to avoid precision loss. */
    base.sortParamsByRequiredFlag = true; /* Sort method arguments to place required parameters before optional parameters. */
    base.useDateTimeOffset = 0; /* Use DateTimeOffset to model date-time properties */
    base.ensureUniqueParams = false; /* Whether to ensure parameter names are unique in an operation (rename parameters that are not). */
    base.optionalMethodArgument = false; /* Optional method argument, e.g. void square(int x=10) (.net 4.0+ only). */
    base.optionalAssemblyInfo = false; /* Generate AssemblyInfo.cs. */
    base.netCoreProjectFile = true; /* Use the new format (.NET Core) for .NET project files (.csproj). */
    base.useCollection = false; /* Deserialize array types to Collection<T> instead of List<T>. */
    base.interfacePrefix = ''; /* Prefix interfaces with a community standard or widely accepted prefix. */
    base.returnICollection = false; /* Return ICollection<T> instead of the concrete type. */
    base.optionalProjectFile = false; /* Generate {PackageName}.csproj. */
    base.variableNamingConvention = 'original'; /* {camelCase, PascalCase, snake_case, original, UPPERCASE} */
    base.modelPropertyNaming = 'original'; /* {camelCase, PascalCase, snake_case, original, UPPERCASE} */
    base.targetFramework = 4; /* The target .NET framework version. */
    base.modelNamePrefix = ''; /* Prefix that will be prepended to all model names. Default is the empty string. */
    base.modelNameSuffix = ''; /* Suffix that will be appended to all model names. Default is the empty string. */
    base.releaseNote = 'Minor update'; /* Release note, default to 'Minor update'. */
    base.supportsES6 = true; /* Generate code that conforms to ES6. */
    base.supportsAsync = true; /* Generate code that supports async operations. */
    base.emitJSDoc = true; /* */
    base.emitModelMethods = true; /* */
    base.excludeTests = false; /* Specifies that no tests are to be generated. */
    base.generateApiDocs = true; /* Not user-configurable. System provided for use in templates. */
    base.generateApiTests = true; /* Specifies that api tests are to be generated. */
    base.generateModelDocs = true; /* Not user-configurable. System provided for use in templates. */
    base.generateModelTests = true; /* Specifies that model tests are to be generated. */
    base.hideGenerationTimestamp = false; /* Hides the generation timestamp when files are generated. */
    base.generatePropertyChanged = true; /* Specifies that models support raising property changed events. */
    base.nonPublicApi = false; /* Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers. */
    base.validatable = true; /* Generates self-validatable models. */
    base.ignoreFileOverride = '.swagger-codegen-ignore'; /* Specifies an override location for the .swagger-codegen-ignore file. Most useful on initial generation. */
    base.removeOperationIdPrefix = false; /* Remove prefix of operationId, e.g. config_getId => getId */
    base.serverPort = 8000;
    base.newline = '\n';
    base.apiDocPath = '';
    base.modelDocPath = '';
    base.classPrefix = 'cls';

    //extensions
    base.modelNaming = 'original'; /* {camelCase, PascalCase, snake_case, original, UPPERCASE} */
    return base;
}

function getPrime(api,defaults) {
    let prime = {};
    prime.classname = api.info.title.toLowerCase().split(' ').join('_').split('-').join('_');
    prime.projectName = prime.classname;
    prime.appVersion = api.info.version;
    prime.apiVersion = api.info.version;
    prime.packageVersion = api.info.version;
    prime.projectVersion = api.info.version;
    prime.version = api.info.version;
    prime.title = api.info.title;
    prime.swaggerVersion = '2.0';
    prime.generatorVersion = require('./package.json').version;
    prime.swaggerCodegenVersion = 'openapi-codegen-v'+prime.generatorVersion;
    prime.appDescription = api.info.description||'No description';
    prime.projectDescription = prime.appDescription;
    prime.classVarName = 'default'; // see issue #21
    prime.exportedName = prime.classname;
    prime.packageTitle = prime.classname; /* Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file. */
    prime.infoEmail = api.info.contact ? api.info.contact.email : null;
    prime.appContact = prime.infoEmail;
    prime.infoUrl = api.info.contact ? api.info.contact.url : null;
    prime.licenseInfo = api.info.license ? api.info.license.name : null;
    prime.licenseUrl = api.info.license ? api.info.license.url : null;
    prime.appName = api.info.title;
    prime.host = ''
    prime.basePath = '/';
    prime.basePathWithoutHost = '/';
    prime.contextPath = '/';
    prime.packageName = 'IO.OpenAPI';
    prime.apiPackage = prime.packageName; /* package for generated api classes */
    prime.generatorPackage = 'IO.OpenAPI';
    prime.invokerPackage = 'IO.OpenAPI'; /* root package for generated code */
    prime.modelPackage = 'IO.OpenAPI'; /* package for generated models */
    prime.package = 'IO.OpenAPI.Api';
    prime.phpInvokerPackage = prime.invokerPackage; /* root package for generated php code */
    prime.perlModuleName = prime.invokerPackage; /* root module name for generated perl code */
    prime.podVersion = '1.0.0';
    prime.pythonPackageName = prime.invokerPackage; /* package name for generated python code */
    prime.clientPackage = 'IO.OpenAPI.Client';
    prime.importPath = 'IO.OpenAPI.Api.Default';
    prime.hasImport = true;
    prime.hasMore = true;
    prime.generatedDate = new Date().toString();
    prime.generatorClass = defaults.configName; // 'class ' prefix?
    prime.fullyQualifiedGeneratorClass = prime.generatorPackage+'.'+prime.generatorClass;
    prime.imports = [ { "import": "IO.OpenAPI.Model.Default" } ];
    prime.name = prime.classname;
    prime.classFilename = prime.classname;
    prime.jsModuleName = prime.classname;
    prime.moduleName = prime.classname;
    prime.jsProjectName = prime.classname;
    prime.baseNamespace = prime.packageName;
    prime.sourceFolder = './out/'+defaults.configName; /* source folder for generated code */
    prime.templateDir = './templates/'+defaults.configName;
    prime.implFolder = prime.sourceFolder; /* folder for generated implementation code */
    prime.library = ''; /* library template (sub-template) */
    prime.packageGuid = uuidv4(); /* The GUID that will be associated with the C# project */
    prime.optionalEmitDefaultValues = false; /* Set DataMember's EmitDefaultValue. */

    prime.packageProductName = prime.projectName; /* Specifies an AssemblyProduct for the .NET Framework global assembly attributes stored in the AssemblyInfo file. */
    prime.packageCompany = 'Smartbear Software'; /* Specifies an AssemblyCompany for the .NET Framework global assembly attributes stored in the AssemblyInfo file. */
    prime.packageAuthors = 'Swagger-Codegen authors'; /* Specifies Authors property in the .NET Core project file. */
    prime.packageCopyright = 'Copyright 2016 Smartbear Software'; /* Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file. */

//    prime.groupId = x; /* groupId in generated pom.xml */
//    prime.artifactId = x; /* artifactId in generated pom.xml */
//    prime.artifactVersion = x; /* artifact version in generated pom.xml */
//    prime.artifactUrl = x; /* artifact URL in generated pom.xml */
//    prime.scmConnection = x; /* SCM connection in generated pom.xml */
//    prime.scmDeveloperConnection = x; /* SCM developer connection in generated pom.xml */
//    prime.scmUrl = x; /* SCM URL in generated pom.xml */

    prime.httpUserAgent = 'OpenAPI-Codegen/'+prime.packageVersion+'/'+defaults.configName; /* HTTP user agent, e.g. codegen_csharp_api_client, default to 'Swagger-Codegen/{packageVersion}}/{language}' */
    return prime;
}

function transform(api, defaults, callback) {
    let base = getBase(); // defaults which are hard-coded

    let lang = (defaults.language||'').toLowerCase();
    if (typeMaps[lang]) typeMap = typeMaps[lang];
    if (reservedWords[lang]) reserved = reservedWords[lang];

    let prime = getPrime(api,defaults); // defaults which depend in some way on the api definition
    let obj = Object.assign({},base,prime,defaults);

    if (defaults.swagger) {
        obj.swagger = defaults.swagger;
    }
    else {
        const container = {};
        container.spec = api;
        container.source = defaults.source;
        let conv = new downconverter(container);
        obj.swagger = conv.convert();
    }

    obj["swagger-yaml"] = yaml.stringify(obj.swagger); // set to original if converted v2.0
    obj["swagger-json"] = JSON.stringify(obj.swagger, null, 2); // set to original if converted 2.0
    obj["openapi-yaml"] = yaml.stringify(api);
    obj["openapi-json"] = JSON.stringify(api, null, 2);

    // openapi3 extensions
    obj.openapi = {};
    obj.openapi.operationCounter = 1;
    obj.openapi.version = api.openapi;
    obj.openapi.servers = api.servers;

    // helper functions (seen in erlang-client)
    obj.qsEncode = function() {
        thisFunc = encodeURIComponent;
        return function(template,context){
            console.warn(util.inspect(template));
            console.warn(util.inspect(this));
        };
    };
    obj.this = function() {
        console.warn('this called');
        return thisFunc(this.paramName);
    };
    obj.length = function() {
        arrayMode = 'length';
        return true;
    };
    obj.capitalize = function() {
        //? seen in akka-scala
        return true;
    };

    let allSecurity = [];
    if (api.components && api.components.securitySchemes) {
        for (let s in api.components.securitySchemes) {
            let entry = {};
            entry[s] = api.components.securitySchemes[s];
            allSecurity.push(entry);
        }
    }
    let authData = getAuthData(allSecurity,api);
    obj = Object.assign(obj,authData);

    api = deref(api,api,{$ref:'x-oldref'});

    obj.messages = [];
    let message = {};
    let vOptions = {anchors:true, lint:defaults.lint};
    if (defaults.stools && defaults.swagger) {
        stools.specs.v2_0.validate(defaults.swagger,function(err,result){
            if (err) console.error(util.inspect(err));
            if (result.errors) {
                for (let e of result.errors) {
                    let message = {};
                    message.level = 'Error';
                    message.elementType = 'Path';
                    message.message = e.message;
                    message.elementId = e.path.join('/');
                    obj.messages.push(message);
                    if (defaults.verbose) console.log(message);
                }
                for (let w of result.warnings) {
                    let message = {};
                    message.level = 'Warning';
                    message.elementType = 'Path';
                    message.message = w.message;
                    message.elementId = w.path.join('/');
                    obj.messages.push(message);
                    if (defaults.verbose) console.log(message);
                }
            }
        });
    }
    else {
        validator(api,vOptions)
        .then(options => {
            message.level = 'Valid';
            message.elementType = 'Context';
            message.elementId = 'None';
            message.message = 'No validation errors detected';
            obj.messages.push(message);
            if (defaults.verbose) console.log(message);
        })
        .catch(ex => {
            message.level = 'Error';
            message.elementType = 'Context';
            message.elementId = vOptions.context.pop();
            message.message = ex.message;
            obj.messages.push(message);
            console.error(message);
        });
    }
    if (api.servers && api.servers.length) {
        let u = api.servers[0].url;
        let up = url.parse(u);
        obj.host = up.host;
        obj.basePath = up.path;
        obj.basePathWithoutHost = up.path;
    }

    obj.consumes = [];
    obj.produces = [];

    obj.apiInfo = {};
    obj.apiInfo.apis = convertToApis(api,obj,defaults);
    obj.apiInfo.paths = convertToPaths(api,obj,defaults);

    obj.produces = convertArray(obj.produces);
    obj.consumes = convertArray(obj.consumes);

    if (defaults.debug) obj.debugOperations = JSON.stringify(obj,null,2);

    obj.models = [];
    if (api.components) {
        for (let s in api.components.schemas) {
            let schema = api.components.schemas[s];
            if (schema !== null) {
                let container = {};
                let model = {};
                model.name = s;
                if (obj.modelNaming === 'snake_case') {
                    model.name = Case.snake(model.name);
                }
                model.classname = model.name;
                model.classVarName = s;
                model.modelJson = safeJson(schema,null,2);
                model.title = schema.title;
                model.unescapedDescription = schema.description;
                model.classFilename = obj.classPrefix+model.name;
                model.modelPackage = model.name;
                model.isEnum = !!schema.enum;
                model.hasEnums = false;
                model.vars = [];
                walkSchema(schema,{},wsGetState,function(schema,parent,state){
                    let entry = {};
                    entry.name = schema.name || schema.title;
                    if (!entry.name && state.property && (state.property.startsWith('properties') ||
                        state.property.startsWith('additionalProperties'))) {
                        entry.name = state.property.split('/')[1];
                    }

                    if (entry.name) {
                        entry.baseName = entry.name.toLowerCase();
                    }

                    if (obj.variableNamingConvention === 'original') {
                        if (obj.modelPropertyNaming === 'snake_case') {
                            entry.name = Case.snake(entry.name);
                        }
                    } else {
                        if (obj.variableNamingConvention === 'snake_case') {
                            entry.baseName = entry.name;
                            entry.name = Case.snake(entry.name);
                        }
                    }

                    if (reserved.indexOf(entry.name)>=0) {
                        entry.name = Case.pascal(entry.name);
                    }

                    entry.getter = Case.camel('get_'+entry.name);
                    entry.setter = Case.camel('set_'+entry.name);
                    entry.description = schema.description||'';
                    entry.unescapedDescription = entry.description;
                    entry.required = (parent.required && parent.required.indexOf(entry.name)>=0)||false;
                    entry.isNotRequired = !entry.required;
                    entry.readOnly = !!schema.readOnly;
                    entry.type = typeMap(getSchemaType(schema), entry.required, schema);
                    entry.dataType = entry.type; //camelCase for imported files
                    entry.datatype = entry.type; //lower for other files
                    entry.jsonSchema = safeJson(schema,null,2);
                    for (let p in schemaProperties) {
                        if (typeof schema[p] !== 'undefined') entry[p] = schema[p];
                    }
                    entry.isEnum = !!schema.enum;
                    entry.isListContainer = schema.type === 'array';
                    entry.isMapContainer = schema.type === 'object';
                    entry.isPrimitiveType = !entry.isListContainer && !entry.isMapContainer;
                    entry.isNotContainer = entry.isPrimitiveType;
                    if (entry.isEnum) entry.isNotContainer = false;
                    entry.isContainer = !entry.isNotContainer;
                    if ((schema.type === 'object') && schema.properties && schema.properties["x-oldref"]) {
                        entry.complexType = schema.properties["x-oldref"].replace('#/components/schemas/','');
                    }
                    if ((schema.type === 'array') && schema.items && schema.items["x-oldref"]) {
                        entry.itemsComplexType = schema.items["x-oldref"].replace('#/components/schemas/','');
                    }
                    entry.dataFormat = schema.format;
                    entry.defaultValue = schema.default;

                    if (entry.isEnum) {
                        model.allowableValues = {};
                        model.allowableValues.enumVars = [];
                        model["allowableValues.values"] = schema.enum;
                        model.allowableValues.values = schema.enum;
                        for (let v of schema.enum) {
                            let e = { name: v, nameInCamelCase: Case.camel(v), value: '"'+v+'"' }; // insane, why aren't the quotes in the template?
                            model.allowableValues.enumVars.push(e);
                        }
                        model.allowableValues.enumVars = convertArray(model.allowableValues.enumVars);
                    }

                    if (entry.name && state.depth<=1) {
                        entry.nameInCamelCase = Case.pascal(entry.name); // for erlang-client
                        entry.datatypeWithEnum = s+'.'+entry.name+'Enum';
                        entry.enumName = entry.name+'Enum';
                        model.hasEnums = true;
                        model.vars.push(entry);
                    }
                });
                model.vars = convertArray(model.vars);
                container.model = model;
                container.importPath = model.name;
                obj.models.push(container);
            }
        }
    }

    if (obj.models.length === 0) {
        obj.models = { isEmpty: true };
    }
    else {
        Object.defineProperty(obj.models, 'isEmpty', {
            enumerable: true,
            value: false
        });
    }

    obj.orderedModels = {};
    Object.keys(obj.models).sort().forEach(function(key) {
        obj.orderedModels[key] = obj.models[key];
    });

    if (defaults.debug) obj.debugModels = JSON.stringify(obj.models,null,2);

    if (callback) callback(null,obj);
    return obj;
}

module.exports = {
    getBase : getBase,
    getPrime : getPrime,
    transform : transform
};

