|Property|Parent|Default|Description|
|---|---|---|---|
|apis||||
|models||||
|supportingFiles||||
|modelTests||||
|modelDocs||||
|apiTests||||
|apiDocs||||
|apiPackage||IO.OpenAPI|package for generated api classes|
|modelPackage||IO.OpenAPI|package for generated models|
|templateDir||./templates/nodejs||
|allowUnicodeIdentifiers|||boolean, toggles whether unicode identifiers are allowed in names or not, default is false|
|invokerPackage||IO.OpenAPI|root package for generated code|
|phpInvokerPackage||IO.OpenAPI|root package for generated php code|
|perlModuleName||IO.OpenAPI|root module name for generated perl code|
|pythonPackageName||IO.OpenAPI|package name for generated python code|
|groupId|||groupId in generated pom.xml|
|artifactId|||artifactId in generated pom.xml|
|artifactVersion|||artifact version in generated pom.xml|
|artifactUrl|||artifact URL in generated pom.xml|
|scmConnection|||SCM connection in generated pom.xml|
|scmDeveloperConnection|||SCM developer connection in generated pom.xml|
|scmUrl|||SCM URL in generated pom.xml|
|developerName|||developer name in generated pom.xml|
|developerEmail|||developer email in generated pom.xml|
|developerOrganization|||developer organization in generated pom.xml|
|developerOrganizationUrl|||developer organization URL in generated pom.xml|
|licenseName||Unlicense|The name of the license|
|licenseUrl||http://www.apache.org/licenses/LICENSE-2.0.html|The URL of the license|
|sourceFolder||./out/nodejs|source folder for generated code|
|implFolder||./out/nodejs|folder for generated implementation code|
|localVariablePrefix|||prefix for generated code members and local variables|
|serializableModel||true|boolean - toggle "implements Serializable" for generated models|
|bigDecimalAsString|||Treat BigDecimal values as Strings to avoid precision loss.|
|library|||library template (sub-template)|
|sortParamsByRequiredFlag||true|Sort method arguments to place required parameters before optional parameters.|
|useDateTimeOffset|||Use DateTimeOffset to model date-time properties|
|ensureUniqueParams|||Whether to ensure parameter names are unique in an operation (rename parameters that are not).|
|projectName||swagger_petstore||
|packageName||IO.OpenAPI||
|packageVersion||1.0.0||
|packageTitle||swagger_petstore|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.|
|packageProductName||swagger_petstore|Specifies an AssemblyProduct for the .NET Framework global assembly attributes stored in the AssemblyInfo file.|
|packageCompany||Smartbear Software|Specifies an AssemblyCompany for the .NET Framework global assembly attributes stored in the AssemblyInfo file.|
|packageAuthors||Swagger-Codegen authors|Specifies Authors property in the .NET Core project file.|
|packageCopyright||Copyright 2016 Smartbear Software|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.|
|podVersion||1.0.0||
|optionalMethodArgument|||Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).|
|optionalAssemblyInfo|||Generate AssemblyInfo.cs.|
|netCoreProjectFile||true|Use the new format (.NET Core) for .NET project files (.csproj).|
|useCollection|||Deserialize array types to Collection<T> instead of List<T>.|
|interfacePrefix|||Prefix interfaces with a community standard or widely accepted prefix.|
|returnICollection|||Return ICollection<T> instead of the concrete type.|
|optionalProjectFile|||Generate {PackageName}.csproj.|
|packageGuid||162a642e-415f-41bf-9487-a11f8f6226ee|The GUID that will be associated with the C# project|
|modelPropertyNaming||original|{camelCase, PascalCase, snake_case, original, UPPERCASE}|
|targetFramework||4|The target .NET framework version.|
|{camelCase, PascalCase, snake_case, original}||||
|enumPropertyNaming|||Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'|
|modelNamePrefix|||Prefix that will be prepended to all model names. Default is the empty string.|
|modelNameSuffix|||Suffix that will be appended to all model names. Default is the empty string.|
|optionalEmitDefaultValues|||Set DataMember's EmitDefaultValue.|
|gitUserId||Mermade|Git user ID, e.g. swagger-api.|
|gitRepoId||openapi-codegen|Git repo ID, e.g. swagger-codegen.|
|releaseNote||Minor update|Release note, default to 'Minor update'.|
|httpUserAgent||OpenAPI-Codegen/1.0.0/nodejs|HTTP user agent, e.g. codegen_csharp_api_client, default to 'Swagger-Codegen/{packageVersion}}/{language}'|
|supportsES6||true|Generate code that conforms to ES6.|
|supportsAsync||true|Generate code that supports async operations.|
|excludeTests|||Specifies that no tests are to be generated.|
|generateApiDocs||true|Not user-configurable. System provided for use in templates.|
|generateApiTests||true|Specifies that api tests are to be generated.|
|generateModelDocs||true|Not user-configurable. System provided for use in templates.|
|generateModelTests||true|Specifies that model tests are to be generated.|
|hideGenerationTimestamp|||Hides the generation timestamp when files are generated.|
|generatePropertyChanged||true|Specifies that models support raising property changed events.|
|nonPublicApi|||Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.|
|validatable||true|Generates self-validatable models.|
|ignoreFileOverride||.swagger-codegen-ignore|Specifies an override location for the .swagger-codegen-ignore file. Most useful on initial generation.|
|removeOperationIdPrefix|||Remove prefix of operationId, e.g. config_getId => getId|
