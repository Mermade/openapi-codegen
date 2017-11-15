# OpenAPI-CodeGen

Experimental port of [swagger-codegen](https://github.com/swagger-api/swagger-codegen) templates to Node.js. This project was initially a 24-hour hackathon. The model adaptor code is entirely original and has been reverse-engineered from documentation and template usage.

**Work in progress**

Supports OpenAPI 3.0.x natively, and Swagger/OpenAPI 2.0 by internal conversion

## Usage

### CLI

```shell
cg [options] {config} {openapi-definition}

Options:
  --help         Show help                                             [boolean]
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

The crossed-off templates have a working (if not tested) configuration in the **configs** directory.

<details>
<summary>Click here to expand...</summary>

* ~~\_common~~ *contains Apache-2.0 and Unlicense licenses*
*  Ada
*  akka-scala
*  android
*  ~~apache2~~ - **needs work**
*  apex
*  aspnetcore
*  bash
*  clojure
*  codegen
*  ~~confluenceWikiDocs~~ - **needs testing**
*  cpprest
*  csharp
*  csharp-dotnet2
*  dart
*  ~~debug~~ - *used for dumping the model state*
*  Eiffel
*  elixir
*  erlang-client
*  erlang-server
*  finch
*  flash
*  flaskConnexion
*  go
*  go-server
*  ~~Groovy~~ - **untested**
*  haskell-http-client
*  haskell-servant
*  ~~htmlDocs~~ - *appears to work*
*  ~~htmlDocs2~~ - *appears to work, no console errors logged*
*  Java
*  JavaInflector
*  JavaJaxRS
*  JavaPlayFramework
*  Javascript
*  Javascript-Closure-Angular
*  JavaSpring
*  JavaVertXServer
*  JMeter
*  kotlin-client
*  lua
*  lumen
*  MSF4J
*  nancyfx
*  ~~nodejs~~ - **tested**
*  objc
*  ~~openapi~~ - *outputs the input definition (in OpenAPI 3.0.x form)*
*  perl
*  php
*  ~~php-silex~~ - **untested**
*  php-symfony
*  pistache-server
*  powershell
*  python
*  qt5cpp
*  r
*  rails5
*  restbed
*  ruby
*  rust
*  rust-server
*  scala
*  scalatra
*  scalaz
*  sinatra
*  slim
*  ~~swagger~~ - *outputs the input definition (in original form if OpenAPI 2.0)*
*  ~~swagger-static~~ - **tested** *template modified to include partials*
*  swift
*  swift3
*  swift4
*  tizen
*  typescript-angular
*  typescript-angularjs
*  typescript-aurelia
*  typescript-fetch
*  typescript-jquery
*  ~~typescript-node~~ - **untested**
*  undertow
*  ~~validator~~ - *uses swagger2openapi's OpenAPI 3.0 validator internally*
*  ze-ph
</details>

## See also

These projects use their own template model, not that of `swagger-codegen`

* https://github.com/wcandillon/swagger-js-codegen
* https://github.com/Cian-Chambliss/swagger-codegen-prepare

