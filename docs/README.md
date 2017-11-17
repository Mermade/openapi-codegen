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
        { "template": "Hello from {{projectName}}", "output": "README.md" },
        { "input": "index.mustache", "output": "docs/index.html" }
    ]
}
```
