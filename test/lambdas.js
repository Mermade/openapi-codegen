'use strict';

const Hogan = require('hogan.js');
const util = require('util');
const should = require('should');

const lambdas = require('../lambdas.js');

const reservedWords = new Set([
    'lowercase',
    'UPPERCASE',
    'snake_case',
    'PascalCase',
    'camelCase',
    'kebab-case'
]);

function escapeReservedWord(text) {
    return "'"+text+"'";
}

function model() {
    let model = {
        generator: {
            reservedWords: reservedWords,
            escapeReservedWord: escapeReservedWord
        },
        lowerCaseVar: 'myvar',
        upperCaseVar: 'MYVAR',
        snakeCaseVar: 'my_var',
        pascalCaseVar: 'MyVar',
        camelCaseVar: 'myVar',
        kebabCaseVar: 'my-var',
        lowerCaseReservedWord: 'LOWERCASE',
        upperCaseReservedWord: 'uppercase',
        snakeCaseReservedWord: 'SnakeCase',
        pascalCaseReservedWord: 'pascalCase',
        camelCaseReservedWord: 'CamelCase',
        kebabCaseReservedWord: 'KebabCase'
    };
    Object.keys(lambdas).forEach(key => model[key] = lambdas[key]);
    return model;
}

function renderTmpl(model, templateStr, partials) {
    let template = Hogan.compile(templateStr);
    return template.render(model,partials);
}

function render(template) {
    return renderTmpl(model(), template, {})
}

async function main(){
    describe('check conversion to lowercase',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#lowercase}}{{lowerCaseVar}}{{/lowercase}}')
            ).be.exactly('myvar');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#lowercase}}{{upperCaseVar}}{{/lowercase}}')
            ).be.exactly('myvar');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#lowercase}}{{snakeCaseVar}}{{/lowercase}}')
            ).be.exactly('my var');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#lowercase}}{{pascalCaseVar}}{{/lowercase}}')
            ).be.exactly('my var');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#lowercase}}{{camelCaseVar}}{{/lowercase}}')
            ).be.exactly('my var');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#lowercase}}{{kebabCaseVar}}{{/lowercase}}')
            ).be.exactly('my var');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#lowercase}}{{lowerCaseReservedWord}}{{/lowercase}}')
            ).be.exactly("'lowercase'");
        })
    });

    describe('check conversion to UPPERCASE',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#uppercase}}{{lowerCaseVar}}{{/uppercase}}')
            ).be.exactly('MYVAR');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#uppercase}}{{upperCaseVar}}{{/uppercase}}')
            ).be.exactly('MYVAR');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#uppercase}}{{snakeCaseVar}}{{/uppercase}}')
            ).be.exactly('MY VAR');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#uppercase}}{{pascalCaseVar}}{{/uppercase}}')
            ).be.exactly('MY VAR');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#uppercase}}{{camelCaseVar}}{{/uppercase}}')
            ).be.exactly('MY VAR');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#uppercase}}{{kebabCaseVar}}{{/uppercase}}')
            ).be.exactly('MY VAR');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#uppercase}}{{upperCaseReservedWord}}{{/uppercase}}')
            ).be.exactly("'UPPERCASE'");
        })
    });

    describe('check conversion to snake_case',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#snakecase}}{{lowerCaseVar}}{{/snakecase}}')
            ).be.exactly('myvar');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#snakecase}}{{upperCaseVar}}{{/snakecase}}')
            ).be.exactly('myvar');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#snakecase}}{{snakeCaseVar}}{{/snakecase}}')
            ).be.exactly('my_var');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#snakecase}}{{pascalCaseVar}}{{/snakecase}}')
            ).be.exactly('my_var');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#snakecase}}{{camelCaseVar}}{{/snakecase}}')
            ).be.exactly('my_var');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#snakecase}}{{kebabCaseVar}}{{/snakecase}}')
            ).be.exactly('my_var');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#snakecase}}{{snakeCaseReservedWord}}{{/snakecase}}')
            ).be.exactly("'snake_case'");
        })
    });

    describe('check conversion to PascalCase',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#pascalcase}}{{lowerCaseVar}}{{/pascalcase}}')
            ).be.exactly('Myvar');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#pascalcase}}{{upperCaseVar}}{{/pascalcase}}')
            ).be.exactly('Myvar');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#pascalcase}}{{snakeCaseVar}}{{/pascalcase}}')
            ).be.exactly('MyVar');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#pascalcase}}{{pascalCaseVar}}{{/pascalcase}}')
            ).be.exactly('MyVar');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#pascalcase}}{{camelCaseVar}}{{/pascalcase}}')
            ).be.exactly('MyVar');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#pascalcase}}{{kebabCaseVar}}{{/pascalcase}}')
            ).be.exactly('MyVar');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#pascalcase}}{{pascalCaseReservedWord}}{{/pascalcase}}')
            ).be.exactly("'PascalCase'");
        })
    });

    describe('check conversion to camelCase',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#camelcase}}{{lowerCaseVar}}{{/camelcase}}')
            ).be.exactly('myvar');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#camelcase}}{{upperCaseVar}}{{/camelcase}}')
            ).be.exactly('myvar');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#camelcase}}{{snakeCaseVar}}{{/camelcase}}')
            ).be.exactly('myVar');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#camelcase}}{{pascalCaseVar}}{{/camelcase}}')
            ).be.exactly('myVar');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#camelcase}}{{camelCaseVar}}{{/camelcase}}')
            ).be.exactly('myVar');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#camelcase}}{{kebabCaseVar}}{{/camelcase}}')
            ).be.exactly('myVar');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#camelcase}}{{camelCaseReservedWord}}{{/camelcase}}')
            ).be.exactly("'camelCase'");
        })
    });

    describe('check conversion to kebab-case',function(){
        it('should convert from lowerCaseVar', function() {
            should(
                render('{{#kebabcase}}{{lowerCaseVar}}{{/kebabcase}}')
            ).be.exactly('myvar');
        })
        it('should convert from upperCaseVar', function() {
            should(
                render('{{#kebabcase}}{{upperCaseVar}}{{/kebabcase}}')
            ).be.exactly('myvar');
        })
        it('should convert from snakeCaseVar', function() {
            should(
                render('{{#kebabcase}}{{snakeCaseVar}}{{/kebabcase}}')
            ).be.exactly('my-var');
        })
        it('should convert from pascalCaseVar', function() {
            should(
                render('{{#kebabcase}}{{pascalCaseVar}}{{/kebabcase}}')
            ).be.exactly('my-var');
        })
        it('should convert from camelCaseVar', function() {
            should(
                render('{{#kebabcase}}{{camelCaseVar}}{{/kebabcase}}')
            ).be.exactly('my-var');
        })
        it('should convert from kebabCaseVar', function() {
            should(
                render('{{#kebabcase}}{{kebabCaseVar}}{{/kebabcase}}')
            ).be.exactly('my-var');
        })
        it('should escape reserved word', function() {
            should(
                render('{{#kebabcase}}{{kebabCaseReservedWord}}{{/kebabcase}}')
            ).be.exactly("'kebab-case'");
        })
    });
    run();
}

main();

