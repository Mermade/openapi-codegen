'use strict';

var utils = require('../utils/writer.js');
var petApi = require('../impl/petApiService');

module.exports.addPet = function addPet (req, res, next) {
  var body = req.swagger.params['body'].value;
  petApi.addPet(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updatePet = function updatePet (req, res, next) {
  var body = req.swagger.params['body'].value;
  petApi.updatePet(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.findPetsByStatus = function findPetsByStatus (req, res, next) {
  var status = req.swagger.params['status'].value;
  petApi.findPetsByStatus(status)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.findPetsByTags = function findPetsByTags (req, res, next) {
  var tags = req.swagger.params['tags'].value;
  petApi.findPetsByTags(tags)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getPetById = function getPetById (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  petApi.getPetById(petId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updatePetWithForm = function updatePetWithForm (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var body = req.swagger.params['body'].value;
  petApi.updatePetWithForm(petId,body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.deletePet = function deletePet (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var api_key = req.swagger.params['api_key'].value;
  petApi.deletePet(petId,api_key)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.uploadFile = function uploadFile (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var body = req.swagger.params['body'].value;
  petApi.uploadFile(petId,body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
