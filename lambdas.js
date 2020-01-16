'use strict';

const Hogan = require('hogan.js');
const Case = require('case');

/**
 * Converts text in a fragment to lowercase.
 *
 * Use:
 * <pre>
 * {{#lowercase}}{{httpMethod}}{{/lowercase}}
 * </pre>
 */

function lowerCase() {
    return function(template) {
        var text = Case.lower(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}


/**
 * Converts text in a fragment to uppercase.
 *
 * Use:
 * <pre>
 * {{#uppercase}}{{summary}}{{/uppercase}}
 * </pre>
 */
function upperCase() {
    return function(template) {
        var text = Case.upper(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}

/**
 * Converts text in a fragment to snake_case.
 *
 * Use:
 * <pre>
 * {{#snakecase}}{{name}}{{/snakecase}}
 * </pre>
 */
function snakeCase() {
    return function(template) {
        var text = Case.snake(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}

/**
 * Converts text in a fragment to PascalCase.
 *
 * Use:
 * <pre>
 * {{#pascalcase}}{{name}}{{/pascalcase}}
 * </pre>
 */
function pascalCase() {
    return function(template) {
        var text = Case.pascal(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}

/**
 * Converts text in a fragment to camelCase.
 *
 * Use:
 * <pre>
 * {{#camelcase}}{{name}}{{/camelcase}}
 * </pre>
 */
function camelCase() {
    return function(template) {
        var text = Case.camel(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}

/**
 * Converts text in a fragment to kebab-case.
 *
 * Use:
 * <pre>
 * {{#kebabcase}}{{name}}{{/kebabcase}}
 * </pre>
 */
function kebabCase() {
    return function(template) {
        var text = Case.kebab(Hogan.compile(template).render(this));
        if (this.generator) {
            text = maybeEscapeReservedWord(this.generator, text)
        }
        return text;
    }
}

function maybeEscapeReservedWord(generator, text) {
    if (generator.sanitizeName) {
        text = generator.sanitizeName(text);
    }
    if (generator.reservedWords && generator.reservedWords.has(text)) {
        // Escaping must be done *after* camelize, because generators
        // may escape using characters removed by camelize function.
        text = generator.escapeReservedWord(text);
    }
    return text
}


module.exports = {
    lowercase: lowerCase,
    uppercase: upperCase,
    snakecase: snakeCase,
    pascalcase: pascalCase,
    camelcase: camelCase,
    kebabcase: kebabCase
}
