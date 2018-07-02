h1. Swagger Petstore This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.

*Version:* 1.0.0

----

{toc:printable=true|style=square|minLevel=2|maxLevel=3|type=list|outline=false|include=.*}

h2. Endpoints
    h3. addPet
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /pet
    {code}
    *Summary:* Add a new pet to the store
    *Description:* 


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |Pet object that needs to be added to the store |(/) | |  |






    h4. Responses
        *Status Code:* 405
        *Message:*     Invalid input
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. updatePet
    {status:colour=Yellow|title=PUT|subtle=false}
    {code}
    PUT /pet
    {code}
    *Summary:* Update an existing pet
    *Description:* 


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |Pet object that needs to be added to the store |(/) | |  |






    h4. Responses
        *Status Code:* 400
        *Message:*     Invalid ID supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     Pet not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 405
        *Message:*     Validation exception
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. findPetsByStatus
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /pet/findByStatus
    {code}
    *Summary:* Finds Pets by status
    *Description:* Multiple status values can be provided with comma separated strings


    h4. Parameters



        h5. Query Parameters
        ||Name||Description||Required||Default||Pattern||
|status |Status values that need to be considered for filter |(/) | |  |




    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
array
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                [
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
]
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "array",
    "items": {
      "type": "object",
      "required": [
        "name",
        "photoUrls"
      ],
      "properties": {
        "id": {
          "type": "integer",
          "format": "int64"
        },
        "category": {
          "type": "object",
          "properties": {
            "id": {
              "type": "integer",
              "format": "int64"
            },
            "name": {
              "type": "string"
            }
          },
          "xml": {
            "name": "Category"
          },
          "x-oldref": "#/components/schemas/Category"
        },
        "name": {
          "type": "string",
          "example": "doggie"
        },
        "photoUrls": {
          "type": "array",
          "xml": {
            "name": "photoUrl",
            "wrapped": true
          },
          "items": {
            "type": "string"
          }
        },
        "tags": {
          "type": "array",
          "xml": {
            "name": "tag",
            "wrapped": true
          },
          "items": {
            "type": "object",
            "properties": {
              "id": {
                "type": "integer",
                "format": "int64"
              },
              "name": {
                "type": "string"
              }
            },
            "xml": {
              "name": "Tag"
            },
            "x-oldref": "#/components/schemas/Tag"
          }
        },
        "status": {
          "type": "string",
          "description": "pet status in the store",
          "enum": [
            "available",
            "pending",
            "sold"
          ]
        }
      },
      "xml": {
        "name": "Pet"
      },
      "x-oldref": "#/components/schemas/Pet"
    }
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
[
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
]
            {code}
        *Status Code:* 400
        *Message:*     Invalid status value
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. findPetsByTags
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /pet/findByTags
    {code}
    *Summary:* Finds Pets by tags
    *Description:* Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.


    h4. Parameters



        h5. Query Parameters
        ||Name||Description||Required||Default||Pattern||
|tags |Tags to filter by |(/) | |  |




    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
array
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                [
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
]
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "array",
    "items": {
      "type": "object",
      "required": [
        "name",
        "photoUrls"
      ],
      "properties": {
        "id": {
          "type": "integer",
          "format": "int64"
        },
        "category": {
          "type": "object",
          "properties": {
            "id": {
              "type": "integer",
              "format": "int64"
            },
            "name": {
              "type": "string"
            }
          },
          "xml": {
            "name": "Category"
          },
          "x-oldref": "#/components/schemas/Category"
        },
        "name": {
          "type": "string",
          "example": "doggie"
        },
        "photoUrls": {
          "type": "array",
          "xml": {
            "name": "photoUrl",
            "wrapped": true
          },
          "items": {
            "type": "string"
          }
        },
        "tags": {
          "type": "array",
          "xml": {
            "name": "tag",
            "wrapped": true
          },
          "items": {
            "type": "object",
            "properties": {
              "id": {
                "type": "integer",
                "format": "int64"
              },
              "name": {
                "type": "string"
              }
            },
            "xml": {
              "name": "Tag"
            },
            "x-oldref": "#/components/schemas/Tag"
          }
        },
        "status": {
          "type": "string",
          "description": "pet status in the store",
          "enum": [
            "available",
            "pending",
            "sold"
          ]
        }
      },
      "xml": {
        "name": "Pet"
      },
      "x-oldref": "#/components/schemas/Pet"
    }
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
[
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
]
            {code}
        *Status Code:* 400
        *Message:*     Invalid tag value
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. getPetById
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /pet/{petId}
    {code}
    *Summary:* Find pet by ID
    *Description:* Returns a single pet


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|petId |ID of pet to return |(/) | |  |







    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
Pet
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
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
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "required": [
      "name",
      "photoUrls"
    ],
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "category": {
        "type": "object",
        "properties": {
          "id": {
            "type": "integer",
            "format": "int64"
          },
          "name": {
            "type": "string"
          }
        },
        "xml": {
          "name": "Category"
        },
        "x-oldref": "#/components/schemas/Category"
      },
      "name": {
        "type": "string",
        "example": "doggie"
      },
      "photoUrls": {
        "type": "array",
        "xml": {
          "name": "photoUrl",
          "wrapped": true
        },
        "items": {
          "type": "string"
        }
      },
      "tags": {
        "type": "array",
        "xml": {
          "name": "tag",
          "wrapped": true
        },
        "items": {
          "type": "object",
          "properties": {
            "id": {
              "type": "integer",
              "format": "int64"
            },
            "name": {
              "type": "string"
            }
          },
          "xml": {
            "name": "Tag"
          },
          "x-oldref": "#/components/schemas/Tag"
        }
      },
      "status": {
        "type": "string",
        "description": "pet status in the store",
        "enum": [
          "available",
          "pending",
          "sold"
        ]
      }
    },
    "xml": {
      "name": "Pet"
    },
    "x-oldref": "#/components/schemas/Pet"
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
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
            {code}
        *Status Code:* 400
        *Message:*     Invalid ID supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     Pet not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. updatePetWithForm
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /pet/{petId}
    {code}
    *Summary:* Updates a pet in the store with form data
    *Description:* 


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|petId |ID of pet that needs to be updated |(/) | |  |

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body | |(x) | |  |






    h4. Responses
        *Status Code:* 405
        *Message:*     Invalid input
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. deletePet
    {status:colour=Yellow|title=DELETE|subtle=false}
    {code}
    DELETE /pet/{petId}
    {code}
    *Summary:* Deletes a pet
    *Description:* 


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|petId |Pet id to delete |(/) | |  |


        h5. Header Parameters
        ||Name||Description||Required||Default||Pattern||





    h4. Responses
        *Status Code:* 400
        *Message:*     Invalid ID supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     Pet not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. uploadFile
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /pet/{petId}/uploadImage
    {code}
    *Summary:* uploads an image
    *Description:* 


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|petId |ID of pet to update |(/) | |  |

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body | |(x) | |  |






    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
ApiResponse
        {code}
        See [#models]

                {code:title=Example application/json |collapse=true }
                {
  "code": 0,
  "type": "string",
  "message": "string"
}
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "properties": {
      "code": {
        "type": "integer",
        "format": "int32"
      },
      "type": {
        "type": "string"
      },
      "message": {
        "type": "string"
      }
    },
    "x-oldref": "#/components/schemas/ApiResponse"
  }
}
        {code}
            {code:title=Example application/json |collapse=true }
{
  "code": 0,
  "type": "string",
  "message": "string"
}
            {code}
    ----
    h3. getInventory
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /store/inventory
    {code}
    *Summary:* Returns pet inventories by status
    *Description:* Returns a map of status codes to quantities


    h4. Parameters







    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
object
        {code}
        See [#models]

                {code:title=Example application/json |collapse=true }
                {
  "property1": 0,
  "property2": 0
}
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "additionalProperties": {
      "type": "integer",
      "format": "int32"
    }
  }
}
        {code}
            {code:title=Example application/json |collapse=true }
{
  "property1": 0,
  "property2": 0
}
            {code}
    ----
    h3. placeOrder
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /store/order
    {code}
    *Summary:* Place an order for a pet
    *Description:* 


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |order placed for purchasing the pet |(/) | |  |






    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
Order
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                {
  "id": 0,
  "petId": 0,
  "quantity": 0,
  "shipDate": "2018-07-02T10:01:50Z",
  "status": "placed",
  "complete": false
}
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "petId": {
        "type": "integer",
        "format": "int64"
      },
      "quantity": {
        "type": "integer",
        "format": "int32"
      },
      "shipDate": {
        "type": "string",
        "format": "date-time"
      },
      "status": {
        "type": "string",
        "description": "Order Status",
        "enum": [
          "placed",
          "approved",
          "delivered"
        ]
      },
      "complete": {
        "type": "boolean",
        "default": false
      }
    },
    "xml": {
      "name": "Order"
    },
    "x-oldref": "#/components/schemas/Order"
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
{
  "id": 0,
  "petId": 0,
  "quantity": 0,
  "shipDate": "2018-07-02T10:01:50Z",
  "status": "placed",
  "complete": false
}
            {code}
        *Status Code:* 400
        *Message:*     Invalid Order
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. getOrderById
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /store/order/{orderId}
    {code}
    *Summary:* Find purchase order by ID
    *Description:* For valid response try integer IDs with value &gt;= 1 and &lt;= 10. Other values will generated exceptions


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|orderId |ID of pet that needs to be fetched |(/) | |  |







    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
Order
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                {
  "id": 0,
  "petId": 0,
  "quantity": 0,
  "shipDate": "2018-07-02T10:01:50Z",
  "status": "placed",
  "complete": false
}
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "petId": {
        "type": "integer",
        "format": "int64"
      },
      "quantity": {
        "type": "integer",
        "format": "int32"
      },
      "shipDate": {
        "type": "string",
        "format": "date-time"
      },
      "status": {
        "type": "string",
        "description": "Order Status",
        "enum": [
          "placed",
          "approved",
          "delivered"
        ]
      },
      "complete": {
        "type": "boolean",
        "default": false
      }
    },
    "xml": {
      "name": "Order"
    },
    "x-oldref": "#/components/schemas/Order"
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
{
  "id": 0,
  "petId": 0,
  "quantity": 0,
  "shipDate": "2018-07-02T10:01:50Z",
  "status": "placed",
  "complete": false
}
            {code}
        *Status Code:* 400
        *Message:*     Invalid ID supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     Order not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. deleteOrder
    {status:colour=Yellow|title=DELETE|subtle=false}
    {code}
    DELETE /store/order/{orderId}
    {code}
    *Summary:* Delete purchase order by ID
    *Description:* For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|orderId |ID of the order that needs to be deleted |(/) | |  |







    h4. Responses
        *Status Code:* 400
        *Message:*     Invalid ID supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     Order not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. createUser
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /user
    {code}
    *Summary:* Create user
    *Description:* This can only be done by the logged in user.


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |Created user object |(/) | |  |






    h4. Responses
        *Status Code:* default
        *Message:*     successful operation
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. createUsersWithArrayInput
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /user/createWithArray
    {code}
    *Summary:* Creates list of users with given input array
    *Description:* 


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |List of user object |(/) | |  |






    h4. Responses
        *Status Code:* default
        *Message:*     successful operation
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. createUsersWithListInput
    {status:colour=Yellow|title=POST|subtle=false}
    {code}
    POST /user/createWithList
    {code}
    *Summary:* Creates list of users with given input array
    *Description:* 


    h4. Parameters

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |List of user object |(/) | |  |






    h4. Responses
        *Status Code:* default
        *Message:*     successful operation
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. loginUser
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /user/login
    {code}
    *Summary:* Logs user into the system
    *Description:* 


    h4. Parameters



        h5. Query Parameters
        ||Name||Description||Required||Default||Pattern||
|username |The user name for login |(/) | |  |
|password |The password for login in clear text |(/) | |  |




    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
string
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                "string"
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "string"
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
"string"
            {code}
        *Status Code:* 400
        *Message:*     Invalid username/password supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. logoutUser
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /user/logout
    {code}
    *Summary:* Logs out current logged in user session
    *Description:* 


    h4. Parameters







    h4. Responses
        *Status Code:* default
        *Message:*     successful operation
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. getUserByName
    {status:colour=Yellow|title=GET|subtle=false}
    {code}
    GET /user/{username}
    {code}
    *Summary:* Get user by user name
    *Description:* 


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|username |The name that needs to be fetched. Use user1 for testing.  |(/) | |  |







    h4. Responses
        *Status Code:* 200
        *Message:*     successful operation
        {code:title=Response Type}
User
        {code}
        See [#models]

                {code:title=Example application/xml |collapse=true }
                {
  "id": 0,
  "username": "string",
  "firstName": "string",
  "lastName": "string",
  "email": "string",
  "password": "string",
  "phone": "string",
  "userStatus": 0
}
{code}


        {code:title=Response Schema |collapse=true}
{
  "schema": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "username": {
        "type": "string"
      },
      "firstName": {
        "type": "string"
      },
      "lastName": {
        "type": "string"
      },
      "email": {
        "type": "string"
      },
      "password": {
        "type": "string"
      },
      "phone": {
        "type": "string"
      },
      "userStatus": {
        "type": "integer",
        "format": "int32",
        "description": "User Status"
      }
    },
    "xml": {
      "name": "User"
    },
    "x-oldref": "#/components/schemas/User"
  }
}
        {code}
            {code:title=Example application/xml |collapse=true }
{
  "id": 0,
  "username": "string",
  "firstName": "string",
  "lastName": "string",
  "email": "string",
  "password": "string",
  "phone": "string",
  "userStatus": 0
}
            {code}
        *Status Code:* 400
        *Message:*     Invalid username supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     User not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. updateUser
    {status:colour=Yellow|title=PUT|subtle=false}
    {code}
    PUT /user/{username}
    {code}
    *Summary:* Updated user
    *Description:* This can only be done by the logged in user.


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|username |name that need to be updated |(/) | |  |

        h5. Body Parameter
        ||Name||Description||Required||Default||Pattern||
|body |Updated user object |(/) | |  |






    h4. Responses
        *Status Code:* 400
        *Message:*     Invalid user supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     User not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----
    h3. deleteUser
    {status:colour=Yellow|title=DELETE|subtle=false}
    {code}
    DELETE /user/{username}
    {code}
    *Summary:* Delete user
    *Description:* This can only be done by the logged in user.


    h4. Parameters
        h5. Path Parameters
        ||Name||Description||Required||Default||Pattern||
|username |The name that needs to be deleted |(/) | |  |







    h4. Responses
        *Status Code:* 400
        *Message:*     Invalid username supplied
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
        *Status Code:* 404
        *Message:*     User not found
        {code:title=Response Type}

        {code}
        See [#models]



        {code:title=Response Schema |collapse=true}
{
  "schema": {}
}
        {code}
    ----

h2. Models

        h3. Order
        ||Field Name||Required||Type||Description||
         |id |(x) |integer | |
 |petId |(x) |integer | |
 |quantity |(x) |integer | |
 |shipDate |(x) |string | |
 |status |(x) |string |Order Status |
 |complete |(x) |boolean | |
        h3. Category
        ||Field Name||Required||Type||Description||
         |id |(x) |integer | |
 |name |(x) |string | |
        h3. User
        ||Field Name||Required||Type||Description||
         |id |(x) |integer | |
 |username |(x) |string | |
 |firstName |(x) |string | |
 |lastName |(x) |string | |
 |email |(x) |string | |
 |password |(x) |string | |
 |phone |(x) |string | |
 |userStatus |(x) |integer |User Status |
        h3. Tag
        ||Field Name||Required||Type||Description||
         |id |(x) |integer | |
 |name |(x) |string | |
        h3. Pet
        ||Field Name||Required||Type||Description||
         |id |(x) |integer | |
 |category |(x) |object | |
 |name | |string | |
 |photoUrls | |array | |
 |tags |(x) |array | |
 |status |(x) |string |pet status in the store |
        h3. ApiResponse
        ||Field Name||Required||Type||Description||
         |code |(x) |integer | |
 |type |(x) |string | |
 |message |(x) |string | |
