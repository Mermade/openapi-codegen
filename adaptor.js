'use strict';

const util = require('util');
const url = require('url');

const yaml = require('js-yaml');
const uuidv4 = require('uuid/v4');
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

function convertArray(arr) {
    let obj = {};
    for (let i=0;i<arr.length;i++) {
        if (i === 0) {
            obj['-first'] = arr[0];
        }
        else if (i === (arr.length-1)) {
            obj['-last'] = arr[i];
        }
        else obj[i] = arr[i];
    }
    return obj;
}

// TODO add html and possibly termcap (https://www.npmjs.com/package/hermit) renderers
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

let typeMap = typeMaps.nop;
let markdownPP = markdownPPs.nop;


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
    //base.gitUserId = x; /* Git user ID, e.g. swagger-api. */
    //base.gitRepoId = x; /* Git repo ID, e.g. swagger-codegen. */
    base.licenseName = 'Unlicense'; /* The name of the license */
    base.licenseUrl = 'https://unlicense.org'; /* The URL of the license */
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
    base.modelPropertyNaming = 'original'; /* {camelCase, PascalCase, snake_case, original, UPPERCASE} */
    base.targetFramework = 4; /* The target .NET framework version. */
    base.modelNamePrefix = ''; /* Prefix that will be prepended to all model names. Default is the empty string. */
    base.modelNameSuffix = ''; /* Suffix that will be appended to all model names. Default is the empty string. */
    base.releaseNote = 'Minor update'; /* Release note, default to 'Minor update'. */
    base.supportsES6 = true; /* Generate code that conforms to ES6. */
    base.supportsAsync = true; /* Generate code that supports async operations. */
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
    return base;
}

function transform(api, defaults) {
    let base = getBase();

    let lang = (defaults.language||'').toLowerCase();
    if (typeMaps[lang]) typeMap = typeMaps[lang];

    let prime = {};
    prime.projectName = api.info.title;
    prime.appVersion = api.info.version;
    prime.apiVersion = api.info.version;
    prime.packageVersion = api.info.version;
    prime.version = api.info.version;
    prime.swaggerVersion = api.openapi;
    prime.swaggerCodegenVersion = 'openapi-codegen-v'+require('./package.json').version;
    prime.appDescription = api.info.description||'No description';
    prime.classname = api.info.title.toLowerCase().split(' ').join('_').split('-').join('_');
    prime.classVarName = 'default'; //? possibly an array of these based on tags (a la widdershins)
    prime.exportedName = prime.classname;
    prime.packageTitle = prime.classname; /* Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file. */
    prime.infoEmail = api.info.contact ? api.info.contact.email : null;
    prime.infoUrl = api.info.contact ? api.info.contact.url : null;
    prime.licenseInfo = api.info.license ? api.info.license.name : null;
    prime.licenseUrl = api.info.license ? api.info.license.url : null;
    prime.appName = api.info.title;
    prime.host = ''
    prime.basePath = '/';
    prime.contextPath = '/';
    prime.packageName = 'IO.OpenAPI';
    prime.apiPackage = prime.packageName; /* package for generated api classes */
    prime.generatorPackage = 'IO.OpenAPI';
    prime.invokerPackage = 'IO.OpenAPI'; /* root package for generated code */
    prime.hasImport = true;
    prime.modelPackage = 'IO.OpenAPI'; /* package for generated models */
    prime.package = 'IO.OpenAPI.Api';
    prime.phpInvokerPackage = prime.invokerPackage; /* root package for generated php code */
    prime.perlModuleName = prime.invokerPackage; /* root module name for generated perl code */
    prime.podVersion = '1.0.0';
    prime.pythonPackageName = prime.invokerPackage; /* package name for generated python code */
    prime.clientPackage = 'IO.OpenAPI.Client';
    prime.importPath = 'IO.OpenAPI.Api.Default';
    prime.hasMore = true;
    prime.generatedDate = new Date().toString();
    prime.generatorClass = defaults.configName; // 'class ' prefix?
    prime.imports = [ { "import": "IO.OpenAPI.Model.Default" } ];
    prime.name = prime.classname;
    prime.classFilename = prime.classname;
    prime.jsModuleName = prime.classname;
    prime.jsProjectName = prime.classname;
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

    let obj = Object.assign({},base,prime,defaults);

    obj["swagger-yaml"] = yaml.safeDump(defaults.swagger || api, {lineWidth:-1}); // set to original if converted v2.0
    obj["swagger-json"] = JSON.stringify(defaults.swagger || api, null, 2); // set to original if converted 2.0
    obj["openapi-yaml"] = yaml.safeDump(api, {lineWidth:-1});
    obj["openapi-json"] = JSON.stringify(api, null, 2);
    
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
        if (api.components) {
            obj.swagger.parameters = api.components.parameters;
            obj.swagger.headers = api.components.headers;
            obj.swagger.responses = api.components.responses;
            obj.swagger.definitions = api.components.schemas;
        }
    }

    // openapi3 extensions
    obj.servers = api.servers;

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
                operation.hasMore = true; // last one gets reset to false
                operation.isResponseBinary = false; //TODO
                operation.baseName = 'Default';
                if (op.tags && op.tags.length) {
                    operation.baseName = op.tags[0];
                }
                operation.produces = [];
                operation.consumes = [];
                for (let pa in op.parameters) {
                    operation.hasParams = true;
                    let param = op.parameters[pa];
                    let parameter = {};
                    parameter.paramName = param.name;
                    parameter.baseName = param.name;
                    parameter.required = param.required;
                    if (!parameter.required) operation.hasOptionalParams = true;
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
                    operation.bodyParam.required = op.requestBody.required;
                    if (!operation.bodyParam.required) operation.hasOptionalParams = true;
                    operation.bodyParam.dataType = typeMap('object',operation.bodyParam.required,{}); // can be changed below
                    operation.bodyParam.description = op.requestBody.description;
                    operation.bodyParam.schema = {};
                    operation.bodyParam.isEnum = false;
                    operation.bodyParam.vendorExtensions = {};
                    if (op.requestBody.content) {
                        let contentType = Object.values(op.requestBody.content)[0];
                        let mt = { mediaType: Object.keys(op.requestBody.content)[0] };
                        operation.consumes.push(mt);
                        operation.hasConsumes = true;
                        operation.bodyParam.schema = contentType.schema;
                        if (contentType.schema.type) {
                            operation.bodyParam.type = contentType.schema.type;
                            operation.bodyParam.dataType = typeMap(contentType.schema.type,operation.bodyParam.required,contentType.schema);
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
                        let mt = {};
                        mt.mediaType = Object.keys(response.content)[0];
                        operation.produces.push(mt);
                        operation.hasProduces = true;
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

                if (obj.sortParamsByRequiredFlag) {
                    operation.allParams = operation.allParams.sort(function(a,b){
                        if (a.required && !b.required) return -1;
                        if (b.required && !a.required) return +1;
                        return 0;
                    });
                }
                if (operation.allParams.length) {
                    operation.allParams[operation.allParams.length-1].hasMore = false;
                }

                operation.consumes = convertArray(operation.consumes);
                operation.produces = convertArray(operation.produces);

                let container = {};
                container.baseName = operation.nickname;
                container.classname = 'cls'+operation.nickname;
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
            model.vars[model.vars.length-1].hasMore = false;
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

