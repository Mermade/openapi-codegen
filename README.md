# OpenAPI-CodeGen

[![Build status](https://travis-ci.org/Mermade/openapi-codegen.svg?branch=master)](https://travis-ci.org/Mermade/openapi-codegen)
[![Greenkeeper badge](https://badges.greenkeeper.io/Mermade/openapi-codegen.svg)](https://greenkeeper.io/)

Node.js-based codegen for OpenAPI documents. This project was initially a 24-hour hackathon. The local model adaptor code is entirely original and has been reverse-engineered from the existing documentation and template usage.

**Work in progress**

Supports OpenAPI 3.0.x natively, and Swagger/OpenAPI 1.2 and 2.0 by internal conversion. Node.js LTS versions are supported.

## Usage

### Installing

`npm i -g openapi-codegen`

or

* clone the repository, and
* `npm i`

or

`npx -p openapi-codegen cg ...`

### CLI

```
cg [options] {[path]configName} {openapi-definition}

Options:
  --help           Show help                                           [boolean]
  --version        Show version number                                 [boolean]
  --filter         Filter term to use with --list                       [string]
  --list           List available templates for provider (og or sc)     [string]
  -d, --debug      Turn on debugging information in the model          [boolean]
  -f, --flat       Do not include config-name in output directory structure
                                                                       [boolean]
  -l, --lint       Lint input definition                               [boolean]
  -o, --output     Specify output directory         [string] [default: "./out/"]
  -s, --stools     Use swagger-tools to validate OpenAPI 2.0 definitions
                                                                       [boolean]
  -t, --templates  Specify templates directory                          [string]
  -v, --verbose    Increase verbosity                                  [boolean]
  -z, --zip        Create a .zip file instead of individual files      [boolean]
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

The local templates were taken directly from `swagger-codegen`. This project is also licensed under [Apache-2.0](LICENSE) for this reason. Generated code is explicitly covered by the [Unlicense](templates/_common/UNLICENSE). Code to downconvert OpenAPI 3.0 definitions is taken from [Angular-Swagger-UI](https://github.com/Orange-OpenSource/angular-swagger-ui) and is MIT licensed.

You can also use the latest online templates from two providers: `og` ([openapi-generator](https://github.com/OpenAPITools/openapi-generator)) and `sc` ([swagger-codegen](https://github.com/swagger-api/swagger-codegen)). The `--list` and `--filter` options allow you to see which templates are available. Note that using the online templates involves sending your API definition to a remote server.

### Contributors

See [here](https://github.com/swagger-api/swagger-codegen#template-creator) for a partial list of template contributors.

### Status of the template configurations

The local templates with a status have a working (if not necessarily tested) configuration in the **configs** directory. Contributions are welcomed from the community of new and updated configurations and template updates.

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
|**csharp-dotnet2**|client|**Untested**||
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
|**python**|client|**needs testing**|||@mpnordland
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
|**typescript-axios**|client|**tested**||jaredpalmer|
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

