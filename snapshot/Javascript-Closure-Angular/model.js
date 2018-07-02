goog.provide('IO.OpenAPI.Api.Order');
goog.provide('IO.OpenAPI.Api.Category');
goog.provide('IO.OpenAPI.Api.User');
goog.provide('IO.OpenAPI.Api.Tag');
goog.provide('IO.OpenAPI.Api.Pet');
goog.provide('IO.OpenAPI.Api.ApiResponse');

/**
 * @record
 */
IO.OpenAPI.Api.Order = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.id;

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.petId;

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.quantity;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.shipDate;

/**
 * Order Status
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.status;

/**
 * @type {!boolean}
 * @export
 */
IO.OpenAPI.Api.Order.prototype.complete;

/** @enum {string} */
IO.OpenAPI.Api.Order.Order.statusEnum = { 
}
/**
 * @record
 */
IO.OpenAPI.Api.Category = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Category.prototype.id;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Category.prototype.name;

/**
 * @record
 */
IO.OpenAPI.Api.User = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.User.prototype.id;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.username;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.firstName;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.lastName;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.email;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.password;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.User.prototype.phone;

/**
 * User Status
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.User.prototype.userStatus;

/**
 * @record
 */
IO.OpenAPI.Api.Tag = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Tag.prototype.id;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Tag.prototype.name;

/**
 * @record
 */
IO.OpenAPI.Api.Pet = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.id;

/**
 * @type {!object}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.category;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.name;

/**
 * @type {!array}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.photoUrls;

/**
 * @type {!array}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.tags;

/**
 * pet status in the store
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.Pet.prototype.status;

/** @enum {string} */
IO.OpenAPI.Api.Pet.Pet.statusEnum = { 
}
/**
 * @record
 */
IO.OpenAPI.Api.ApiResponse = function() {}

/**
 * @type {!integer}
 * @export
 */
IO.OpenAPI.Api.ApiResponse.prototype.code;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.ApiResponse.prototype.type;

/**
 * @type {!string}
 * @export
 */
IO.OpenAPI.Api.ApiResponse.prototype.message;

