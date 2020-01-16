# OpenAPI CodeGen

## Documentation

### Config file DSL

#### Schema

[JSON Schema Draft 4](https://raw.githubusercontent.com/Mermade/openapi-codegen/master/schemas/config.json)

#### Example

```json
{
    "type": "documentation",
    "defaults": {
      "exampleProperty": "exampleValue"
    },
    "directories": [
        "docs"
    ],
    "partials": {
        "model": "model.mustache",
        "operation": "operation.mustache"
    },
    "transformations": [
        { "template": "Hello from \{\{projectName\}\}", "output": "README.md" },
        { "input": "index.mustache", "output": "docs/index.html" }
    ]
}
```

### Model properties

* [Defaults](modelProperties.md)

### Predefined lambdas

Lambdas are special tags which invoke a predefined function named after tag.
The function would receive the template fragement between the tag. More info
about lambdas could be found [here](https://github.com/OpenAPITools/openapi-generator/blob/master/docs/templating.md#mustache-lambdas)

We support following lambdas at the moment

| lambda     | example                                | description           |
|------------|----------------------------------------|-----------------------|
| lowercase  | {{#lowercase}}{{name}}{{/lowercase}}   | Convert to lowercase  |
| uppercase  | {{#uppercase}}{{name}}{{/uppercase}}   | Convert to UPPERCASE  |
| snakecase  | {{#snakecase}}{{name}}{{/snacecase}}   | Convert to snake_case |
| pascalcase | {{#pascalcase}}{{name}}{{/pascalcase}} | Convert to PascalCase |
| camelcase  | {{#camelcase}}{{name}}{{/camelcase}}   | Convert to camelCase  |
| kebabcase  | {{#kebabcase}}{{name}}{{/kebabcase}}   | Convert to kebab-case |

### Custom generators

Some languages have various reserved words or weird way of arguments formating.
Sometimes custom lambdas are needed to solve the issues.
This means there should be a way to customize the codegeration. Custom generator
is a javascript module defined as:

```
const Hogan = require('hogan.js');

const RESERVED_WORDS = new Set([
    'for'
]);

function sanitizeName(text) {
    return "__"+text+"__"
}

function escapeReservedWord(text) {
    return "'"+text+"'"
}

function hello_lambda() {
    return function(text) {
      return 'Hello ' + Hogan.compile(text).render(this);
    }
};

function complex_lambda() {
    return function(template) {
        var path = Hogan.compile(template).render(this);
        path = this.pathParams.reduce(function (acc, param) {
            if (param.isPathParam) {
               return acc.replace(
                   "{"+param.paramName+"}", param.paramNamePascalCase);
            } else {
                return acc.replace()
            }
        }, path);
        return path.split("/").slice(1).join(", ")
    }
}

module.exports = {
    sanitizeName: sanitizeName,
    reservedWords: RESERVED_WORDS,
    escapeReservedWord: escapeReservedWord,
    lambdas: {
        hello: hello_lambda,
        path_template: complex_lambda
    }
}
```

The module has to be configured in config under `generator` property. Here is an example:
```
{
    "defaults": {
        "modelNaming": "snake_case",
    },
    "generator": "../mygenerator.js",
    "partials": {
    },
    "directories": [ "src" ],
    "transformations": [
    ],
    "perApi": [
    ],
    "perModel": [
    ]
}

```