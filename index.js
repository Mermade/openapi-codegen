'use strict';

const mustache = require('mustache');

function render(template, model, partials) {
    return mustache.render(template, model, partials);
}

module.exports = {
    render : render
};

