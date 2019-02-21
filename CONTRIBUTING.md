# Contributing

First of all, many thanks for considering making a contribution to this project.

## Submitting PRs

Please **do not** include any changes to `package-lock.json` in your PRs, even if you are updating `package.json`. These changes are almost impossible to review for security implications. If necessary, `package-lock.json` will be regenerated and committed by a maintainer after your PR is merged.

## Configs

The main thing we need is config files which control the process of which files are processed by a particular set of templates. Testing
the generated code is also extremely valuable.

## New templates

In the first instance, templates should be submitted to the parent [swagger-codegen](https://github.com/swagger-api/swagger-codegen)
project. If this is not possible, new templates can be added here first.

## TODO items

* [GitHub search](https://github.com/Mermade/openapi-codegen/search?utf8=%E2%9C%93&q=todo+language%3Ajavascript&type=)

## Known bugs

* [GitHub search](https://github.com/Mermade/openapi-codegen/search?utf8=%E2%9C%93&q=fixme+language%3Ajavascript&type=)
