'use strict';

var utils = require('../utils/writer.js');
var clientOptions = require('../services/gclientOptionsService');

module.exports.clientOptions = function clientOptions (req, res, next) {
  clientOptions.clientOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var getClientOptions = require('../services/ggetClientOptionsService');

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
var generateClient = require('../services/ggenerateClientService');

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
var downloadFile = require('../services/gdownloadFileService');

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
var serverOptions = require('../services/gserverOptionsService');

module.exports.serverOptions = function serverOptions (req, res, next) {
  serverOptions.serverOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
var getServerOptions = require('../services/ggetServerOptionsService');

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
var generateServerForLanguage = require('../services/ggenerateServerForLanguageService');

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
