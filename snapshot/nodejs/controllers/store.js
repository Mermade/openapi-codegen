'use strict';

var utils = require('../utils/writer.js');
var storeApi = require('../impl/storeApiService');

module.exports.getInventory = function getInventory (req, res, next) {
  storeApi.getInventory()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.placeOrder = function placeOrder (req, res, next) {
  var body = req.swagger.params['body'].value;
  storeApi.placeOrder(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getOrderById = function getOrderById (req, res, next) {
  var orderId = req.swagger.params['orderId'].value;
  storeApi.getOrderById(orderId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.deleteOrder = function deleteOrder (req, res, next) {
  var orderId = req.swagger.params['orderId'].value;
  storeApi.deleteOrder(orderId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
