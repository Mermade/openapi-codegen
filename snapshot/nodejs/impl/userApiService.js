'use strict';


/**
 * Create user
 * This can only be done by the logged in user.
 *
 * body struct{} Created user object
 * no response value expected for this operation
 **/
exports.createUser = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Creates list of users with given input array
 * 
 *
 * body [100]struct{} List of user object
 * no response value expected for this operation
 **/
exports.createUsersWithArrayInput = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Creates list of users with given input array
 * 
 *
 * body [100]struct{} List of user object
 * no response value expected for this operation
 **/
exports.createUsersWithListInput = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Logs user into the system
 * 
 *
 * username string The user name for login
 * password string The password for login in clear text
 * returns string
 **/
exports.loginUser = function(username,password) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/xml'] = "string";
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Logs out current logged in user session
 * 
 *
 * no response value expected for this operation
 **/
exports.logoutUser = function() {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Get user by user name
 * 
 *
 * username string The name that needs to be fetched. Use user1 for testing. 
 * returns User
 **/
exports.getUserByName = function(username) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/xml'] = {
  "id": 0,
  "username": "string",
  "firstName": "string",
  "lastName": "string",
  "email": "string",
  "password": "string",
  "phone": "string",
  "userStatus": 0
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Updated user
 * This can only be done by the logged in user.
 *
 * username string name that need to be updated
 * body struct{} Updated user object
 * no response value expected for this operation
 **/
exports.updateUser = function(username,body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Delete user
 * This can only be done by the logged in user.
 *
 * username string The name that needs to be deleted
 * no response value expected for this operation
 **/
exports.deleteUser = function(username) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}

