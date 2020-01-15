// @ts-check
'use strict';

const fetch = require('node-fetch');
const util = require('util');

function main(obj, config, configName, callback) {
}

function format(templates, prefix, type) {
    for (let template of templates) {
        console.log(prefix+':'+type+':'+template);
    }
}

async function slurp(server, prefix, type) {
    await fetch(server+type+'s')
    .then(res => {
        return res.text();
    })
    .then(data => {
        format(JSON.parse(data), prefix, type);
    })
    .catch(err => {
        console.error(util.inspect(err));
    });
}

async function list(prefix) {
    let server = '';
    if (prefix === 'og') {
       server = 'https://api.openapi-generator.tech/api/gen/';
    }
    else if (prefix === 'sc') {
        server = 'https://generator.swagger.io/api/gen/';
    }
    await slurp(server, prefix, 'client');
    await slurp(server, prefix, 'server');
}

module.exports = {
    main,
    list
};

