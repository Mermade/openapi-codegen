'use strict';

var utils = require('../utils/writer.js');
var clientsApi = require('../impl/clientsApiService');

module.exports.clientOptions = function clientOptions (req, res, next) {
  clientsApi.clientOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getClientOptions = function getClientOptions (req, res, next) {
  var language = req.swagger.params['language'].value;
  clientsApi.getClientOptions(language)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.generateClient = function generateClient (req, res, next) {
  var language = req.swagger.params['language'].value;
  var body = req.swagger.params['body'].value;
  clientsApi.generateClient(language,body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.downloadFile = function downloadFile (req, res, next) {
  var fileId = req.swagger.params['fileId'].value;
  clientsApi.downloadFile(fileId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
