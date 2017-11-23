# OpenAPI-CodeGen

[![Build status](https://travis-ci.org/Mermade/openapi-codegen.svg?branch=master)](https://travis-ci.org/Mermade/openapi-codegen)

Experimental port of [swagger-codegen](https://github.com/swagger-api/swagger-codegen) templates to Node.js. This project was initially a 24-hour hackathon. The model adaptor code is entirely original and has been reverse-engineered from the existing documentation and template usage.

**Work in progress**

Supports OpenAPI 3.0.x natively, and Swagger/OpenAPI 2.0 by internal conversion

## Usage

### CLI

```
cg [options] {configName} {openapi-definition}

Options:
  -d, --debug    Turn on debugging information in the model            [boolean]
  --help         Show help                                             [boolean]
  -l, --lint     Lint input definition                                 [boolean]
  -s, --stools   Use swagger-tools to validate OpenAPI 2.0 definitions [boolean]
  --version      Show version number                                   [boolean]
  -v, --verbose  Increase verbosity                                    [boolean]
```

e.g.

```
node cg --verbose nodejs defs/generator.yaml
```

In this case, the generated code will be written to the `.out/nodejs` directory.

You can also load the OpenAPI definition from a URL.

### API

```javascript
const renderer = require('openapi-codegen');
// load a config and a definition
renderer.main(definition,config,configName);
```

## Templates

Templates are taken directly from `swagger-codegen`. This project is also licensed under [Apache-2.0](LICENSE) for this reason. Generated code is explicitly covered by the [Unlicense](templates/_common/UNLICENSE).

### Contributors

See [here](https://github.com/swagger-api/swagger-codegen#template-creator) for a partial list of template contributors.

### Status of the template configurations

The templates with a status have a working (if not necessarily tested) configuration in the **configs** directory.

<details>
<summary>Click here to expand...</summary>

|Template|Type|Status|README|Authors (TODO)|Config Maintainer|
|---|---|---|---|---|---|
|**\_common**|meta| *contains Apache-2.0 and Unlicense licenses*||
|**Ada**|client|**Untested**
|akka-scala||
|android||
|**apache2**|configuration|**needs work**||
|apex||
|aspnetcore||
|**bash**|client|**Syntax ok, needs testing**||@bkryza|@MikeRalphson
|**clojure**|client|**Untested**|
|**codegen**|meta|**Demo only**|||@MikeRalphson
|**confluenceWikiDocs**|documentation|**Tested** with Docker [server](https://hub.docker.com/r/atlassian/confluence-server/)||
|cpprest||
|csharp||
|csharp-dotnet2||
|dart||
|**debug**|meta|*used for dumping the model state*||@Mermade|@MikeRalphson
|Eiffel||
|elixir||
|**erlang-client**|client|**Untested**||
|erlang-server|server|
|finch||
|flash||
|**flaskConnexion**|server|**Needs testing**||
|**go**|client|**Builds, needs testing**||
|**go-server**|server|**Builds and runs**||
|**Groovy**|?|**untested**||
|haskell-http-client|client||||
|**haskell-servant**|server|**Untested**||
|**htmlDocs**|documentation|*Appears to work*||
|**htmlDocs2**|documentation|*Appears to work, no console errors logged*||
|Java||
|JavaInflector||
|JavaJaxRS||
|JavaPlayFramework||
|**Javascript**|client|**Untested**||
|**Javascript-Closure-Angular**|client|**Untested**
|JavaSpring||
|JavaVertXServer||
|**JMeter**|meta|**Untested**||
|kotlin-client||
|**lua**|client|**Compiles OK**|
|lumen||
|MSF4J||
|nancyfx||
|**nodejs**|server|**tested** :white_check_mark:||@jfiala|@MikeRalphson|
|objc||
|**openapi**|meta|*outputs the input definition (in OpenAPI 3.0.x form)* :white_check_mark:||@Mermade|@MikeRalphson
|perl||
|php||
|**php-silex**|?|**untested**||
|php-symfony||
|pistache-server||
|powershell||
|**python**|?|**needs testing**||
|qt5cpp||
|r||
|rails5||
|**restbed**|server|**Untested**||
|ruby||
|rust||
|rust-server||
|scala||
|scalatra||
|scalaz|client|**Untested**||
|**sinatra**|server|**Syntax checks OK**||
|**slim**|server|**Untested**||
|**swagger**|meta|*outputs the input definition (in original form if OpenAPI 2.0)* :white_check_mark:||
|**swagger-static**|documentation|**tested** *template modified to include partials*||
|swift||
|swift3||
|swift4||
|tizen||
|typescript-angular||
|typescript-angularjs||
|typescript-aurelia||
|**typescript-fetch**|client|**compiles with tsc ok**||
|typescript-jquery||
|**typescript-node**|client|**compiles with tsc ok**||
|undertow||
|**validator**|meta|*uses swagger2openapi's OpenAPI 3.0 validator internally* :white_check_mark:||
|ze-ph|

### New Templates

These templates are examples of how features of OpenAPI Codegen may be used, and best-practices in naming model properties.

|Template|Type|Status|README|Authors|Config Maintainer|
|---|---|---|---|---|---|
|testing.dredd|testing|**In progress**|[README](templates/testing.dredd/README.md.mustache)|@Mermade|@MikeRalphson|
</details>

## Documentation

* [See here](docs/README.md) - contributions welcome

## See also

These projects use their own template model, not that of `swagger-codegen`

* https://github.com/fmvilas/swagger-node-codegen
* https://github.com/wcandillon/swagger-js-codegen
* https://github.com/Cian-Chambliss/swagger-codegen-prepare

