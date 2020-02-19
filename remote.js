// @ts-check
'use strict';

const fetch = require('node-fetch');
const util = require('util');

function getServer(prefix) {
    if (prefix === 'og') {
        return 'https://api.openapi-generator.tech/api/gen/';
    }
    else if (prefix === 'sc') {
        return 'https://generator.swagger.io/api/gen/';
    }
    console.warn('Unknown API provider prefix',prefix);
    return false;
}

async function main(obj, config, configName, callback) {
    const components = configName.split(':');
    const prefix = components[0];
    const type = components[1];
    const template = components[2];
    const server = getServer(prefix);
    const body = { options: {}, spec: obj };
    const response = await fetch(server+type+'s/'+template, {
        method: 'post',
        body:    JSON.stringify(body),
        headers: { 'Content-Type': 'application/json' }
    })
    .then(res => res.json())
    .then(json => json)
    .catch(ex => {
      console.warn(ex.message);
    });
    if (response && response.link) {
        const zipfile = await fetch(response.link)
        .then(res => res.buffer())
        .then(buffer => buffer);
        callback(null, zipfile);
    }
    else {
        if (response) console.warn(util.inspect(response));
    }
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
    const server = getServer(prefix);
    if (server) {
        await slurp(server, prefix, 'client', 'clients', filter);
        await slurp(server, prefix, 'server', 'servers', filter);
        return 0;
    }
    return 1;
}

module.exports = {
    main,
    list
};

