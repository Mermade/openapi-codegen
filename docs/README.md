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

Lambdas are special tags which invoke a predefined function named after the tag.
The function receives the template fragment between the tags. More info
about lambdas can be found [here](https://github.com/OpenAPITools/openapi-generator/blob/master/docs/templating.md#mustache-lambdas)

We support the following pre-defined lambdas at the moment:

| lambda     | example                                | description           |
|------------|----------------------------------------|-----------------------|
| lowercase  | {{#lowercase}}{{name}}{{/lowercase}}   | Convert to lowercase  |
| uppercase  | {{#uppercase}}{{name}}{{/uppercase}}   | Convert to UPPERCASE  |
| snakecase  | {{#snakecase}}{{name}}{{/snakecase}}   | Convert to snake_case |
| pascalcase | {{#pascalcase}}{{name}}{{/pascalcase}} | Convert to PascalCase |
| camelcase  | {{#camelcase}}{{name}}{{/camelcase}}   | Convert to camelCase  |
| kebabcase  | {{#kebabcase}}{{name}}{{/kebabcase}}   | Convert to kebab-case |

### Custom generators

Some languages have various reserved words or unusual way of formatting arguments.
Sometimes custom lambdas are needed to solve the issues.
A custom generator is simply a javascript module, for example:

```js
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
}

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
};
```

The module has to be configured in your config under the `generator` property. Here is an example:
```json
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

