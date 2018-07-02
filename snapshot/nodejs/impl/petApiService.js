'use strict';


/**
 * Add a new pet to the store
 * 
 *
 * body struct{} Pet object that needs to be added to the store
 * no response value expected for this operation
 **/
exports.addPet = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Update an existing pet
 * 
 *
 * body struct{} Pet object that needs to be added to the store
 * no response value expected for this operation
 **/
exports.updatePet = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Finds Pets by status
 * Multiple status values can be provided with comma separated strings
 *
 * status [100]string Status values that need to be considered for filter
 * returns [100]struct{}
 **/
exports.findPetsByStatus = function(status) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/xml'] = [
  {
    "id": 0,
    "category": {
      "id": 0,
      "name": "string"
    },
    "name": "doggie",
    "photoUrls": [
      "string"
    ],
    "tags": [
      {
        "id": 0,
        "name": "string"
      }
    ],
    "status": "available"
  }
];
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Finds Pets by tags
 * Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 *
 * tags [100]string Tags to filter by
 * returns [100]struct{}
 **/
exports.findPetsByTags = function(tags) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/xml'] = [
  {
    "id": 0,
    "category": {
      "id": 0,
      "name": "string"
    },
    "name": "doggie",
    "photoUrls": [
      "string"
    ],
    "tags": [
      {
        "id": 0,
        "name": "string"
      }
    ],
    "status": "available"
  }
];
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Find pet by ID
 * Returns a single pet
 *
 * petId int ID of pet to return
 * returns Pet
 **/
exports.getPetById = function(petId) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/xml'] = {
  "id": 0,
  "category": {
    "id": 0,
    "name": "string"
  },
  "name": "doggie",
  "photoUrls": [
    "string"
  ],
  "tags": [
    {
      "id": 0,
      "name": "string"
    }
  ],
  "status": "available"
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Updates a pet in the store with form data
 * 
 *
 * petId int ID of pet that needs to be updated
 * body struct{}  (optional)
 * no response value expected for this operation
 **/
exports.updatePetWithForm = function(petId,body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Deletes a pet
 * 
 *
 * petId int Pet id to delete
 * api_key string  (optional)
 * no response value expected for this operation
 **/
exports.deletePet = function(petId,api_key) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * uploads an image
 * 
 *
 * petId int ID of pet to update
 * body string  (optional)
 * returns ApiResponse
 **/
exports.uploadFile = function(petId,body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = {
  "code": 0,
  "type": "string",
  "message": "string"
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}

