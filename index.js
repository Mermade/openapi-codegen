'use strict';

const mustache = require('mustache');

function render(template, model) {
    return mustache.render(template, model);
}

module.exports = {
    render : render
};

