'use strict';


/**
 * Gets languages supported by the server generator
 * 
 *
 * returns array
 **/
exports.serverOptions = function() {
  return new Promise(function(resolve, reject) {
    var examples = {};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Returns options for a server framework
 * 
 *
 * framework string The target language for the server framework
 * returns object
 **/
exports.getServerOptions = function(framework) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Generates a server library
 * Accepts a `GeneratorInput` options map for spec location and generation options.
 *
 * framework string framework
 * body object parameters
 * returns ResponseCode
 **/
exports.generateServerForLanguage = function(framework,body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}

