'use strict';

var utils = require('../utils/writer.js');
var clientOptions = require('../nodejs/clientOptionsService');

module.exports.clientOptions = function clientOptions (req, res, next) {
  clientOptions.clientOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var getClientOptions = require('../nodejs/getClientOptionsService');

module.exports.getClientOptions = function getClientOptions (req, res, next) {
  var language = req.swagger.params['language'].value;
  getClientOptions.getClientOptions(language)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var generateClient = require('../nodejs/generateClientService');

module.exports.generateClient = function generateClient (req, res, next) {
  var language = req.swagger.params['language'].value;
  generateClient.generateClient(language)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var downloadFile = require('../nodejs/downloadFileService');

module.exports.downloadFile = function downloadFile (req, res, next) {
  var fileId = req.swagger.params['fileId'].value;
  downloadFile.downloadFile(fileId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var serverOptions = require('../nodejs/serverOptionsService');

module.exports.serverOptions = function serverOptions (req, res, next) {
  serverOptions.serverOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var getServerOptions = require('../nodejs/getServerOptionsService');

module.exports.getServerOptions = function getServerOptions (req, res, next) {
  var framework = req.swagger.params['framework'].value;
  getServerOptions.getServerOptions(framework)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var generateServerForLanguage = require('../nodejs/generateServerForLanguageService');

module.exports.generateServerForLanguage = function generateServerForLanguage (req, res, next) {
  var framework = req.swagger.params['framework'].value;
  generateServerForLanguage.generateServerForLanguage(framework)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
