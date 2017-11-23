'use strict';

var utils = require('../utils/writer.js');
var serversApi = require('../impl/serversApiService');

module.exports.serverOptions = function serverOptions (req, res, next) {
  serversApi.serverOptions()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getServerOptions = function getServerOptions (req, res, next) {
  var framework = req.swagger.params['framework'].value;
  serversApi.getServerOptions(framework)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.generateServerForLanguage = function generateServerForLanguage (req, res, next) {
  var framework = req.swagger.params['framework'].value;
  var body = req.swagger.params['body'].value;
  serversApi.generateServerForLanguage(framework,body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
