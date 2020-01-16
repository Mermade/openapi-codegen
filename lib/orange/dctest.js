'use strict';

const yaml = require('js-yaml');
const fs = require('fs');
const converter = require('./downconvert.js');

const input = process.argv[2];
if (input) {
	const swagger = yaml.safeLoad(fs.readFileSync(input,'utf8'));
	const openapi = converter(swagger);
	console.log(yaml.dump(openapi));
}
