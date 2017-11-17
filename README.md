# OpenAPI-CodeGen

[![Build status](https://travis-ci.org/Mermade/openapi-codegen.svg?branch=master)](https://travis-ci.org/Mermade/openapi-codegen)

Experimental port of [swagger-codegen](https://github.com/swagger-api/swagger-codegen) templates to Node.js. This project was initially a 24-hour hackathon. The model adaptor code is entirely original and has been reverse-engineered from the existing documentation and template usage.

**Work in progress**

Supports OpenAPI 3.0.x natively, and Swagger/OpenAPI 2.0 by internal conversion

## Usage

### CLI

```shell
cg [options] {configName} {openapi-definition}

Options:
  -d, --debug    Turn on debugging information in the model            [boolean]
  --help         Show help                                             [boolean]
  -l, --lint     Lint input definition                                 [boolean]
  --version      Show version number                                   [boolean]
  -v, --verbose  Increase verbosity                                    [boolean]
```

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
|Ada||
|akka-scala||
|android||
|**apache2**|configuration|**needs work**||
|apex||
|aspnetcore||
|bash|client|**needs testing**||@bkryza|@MikeRalphson
|clojure||
|codegen||
|**confluenceWikiDocs**|documentation|**needs testing**||
|cpprest||
|csharp||
|csharp-dotnet2||
|dart||
|**debug**|meta|*used for dumping the model state*||@Mermade|@MikeRalphson
|Eiffel||
|elixir||
|erlang-client||
|erlang-server||
|finch||
|flash||
|**flaskConnexion**|server|**needs testing**||
|go||
|go-server||
|**Groovy**|?|**untested**||
|haskell-http-client||
|haskell-servant||
|**htmlDocs**|documentation|*appears to work*||
|**htmlDocs2**|documentation|*appears to work, no console errors logged*||
|Java||
|JavaInflector||
|JavaJaxRS||
|JavaPlayFramework||
|Javascript||
|Javascript-Closure-Angular||
|JavaSpring||
|JavaVertXServer||
|JMeter||
|kotlin-client||
|lua||
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
|restbed||
|ruby||
|rust||
|rust-server||
|scala||
|scalatra||
|scalaz||
|sinatra||
|slim||
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
</details>

## See also

These projects use their own template model, not that of `swagger-codegen`

* https://github.com/wcandillon/swagger-js-codegen
* https://github.com/Cian-Chambliss/swagger-codegen-prepare

