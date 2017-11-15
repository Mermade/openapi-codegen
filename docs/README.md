## Documentation

### Config file DSL

#### Example

```json
{
    "defaults": {
      "exampleProperty": "exampleValue"
    },
    "directories": [
        "docs", "docs/assets", "docs/assets/css", "docs/assets/js", "docs/assets/images"
    ],
    "partials": {
        "model": "model.mustache",
        "operation": "operation.mustache"
    },
    "transformations": [
        { "input": "index.mustache", "output": "docs/index.html" },
    ]
}
```
