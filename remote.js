// @ts-check
'use strict';

const fetch = require('node-fetch');
const util = require('util');

function main(obj, config, configName, callback) {
}

function format(templates, prefix, type, filter) {
    for (let template of templates) {
        if (!filter || (template.indexOf(filter)>-1)) {
            console.log(prefix+':'+type+':'+template);
        }
    }
}

async function slurp(server, prefix, type, plural, filter) {
    await fetch(server+plural)
    .then(res => {
        return res.text();
    })
    .then(data => {
        format(JSON.parse(data), prefix, type, filter);
    })
    .catch(err => {
        console.error(util.inspect(err));
    });
}

async function list(prefix, filter) {
    let server = '';
    if (prefix === 'og') {
       server = 'https://api.openapi-generator.tech/api/gen/';
    }
    else if (prefix === 'sc') {
        server = 'https://generator.swagger.io/api/gen/';
    }
    else {
        console.warn('Unknown API provider prefix',prefix);
        return 1;
    }
    await slurp(server, prefix, 'client', 'clients', filter);
    await slurp(server, prefix, 'server', 'servers', filter);
    return 0;
}

module.exports = {
    main,
    list
};

