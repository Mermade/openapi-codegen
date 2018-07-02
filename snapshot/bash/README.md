# Swagger Petstore Bash client

## Overview
This is a Bash client script for accessing Swagger Petstore service.

The script uses cURL underneath for making all REST calls.

## Usage

```shell
# Make sure the script has executable rights
$ chmod u+x bash_completion.sh

# Print the list of operations available on the service
$ ./bash_completion.sh -h

# Print the service description
$ ./bash_completion.sh --about

# Print detailed information about specific operation
$ ./bash_completion.sh <operationId> -h

# Make GET request
./bash_completion.sh --host http://<hostname>:<port> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make GET request using arbitrary curl options (must be passed before <operationId>) to an SSL service using username:password
bash_completion.sh -k -sS --tlsv1.2 --host https://<hostname> -u <user>:<password> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make POST request
$ echo '<body_content>' | bash_completion.sh --host <hostname> --content-type json <operationId> -

# Make POST request with simple JSON content, e.g.:
# {
#   "key1": "value1",
#   "key2": "value2",
#   "key3": 23
# }
$ echo '<body_content>' | bash_completion.sh --host <hostname> --content-type json <operationId> key1==value1 key2=value2 key3:=23 -

# Preview the cURL command without actually executing it
$ bash_completion.sh --host http://<hostname>:<port> --dry-run <operationid>

```

## Docker image
You can easily create a Docker image containing a preconfigured environment
for using the REST Bash client including working autocompletion and short
welcome message with basic instructions, using the generated Dockerfile:

```shell
docker build -t my-rest-client .
docker run -it my-rest-client
```

By default you will be logged into a Zsh environment which has much more
advanced auto completion, but you can switch to Bash, where basic autocompletion
is also available.

## Shell completion

### Bash
The generated bash-completion script can be either directly loaded to the current Bash session using:

```shell
source bash_completion.sh.bash-completion
```

Alternatively, the script can be copied to the `/etc/bash-completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash-completion.d`):

```shell
sudo cp bash_completion.sh.bash-completion /etc/bash-completion.d/bash_completion.sh
```

#### OS X
On OSX you might need to install bash-completion using Homebrew:
```shell
brew install bash-completion
```
and add the following to the `~/.bashrc`:

```shell
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
```

### Zsh
In Zsh, the generated `_bash_completion.sh` Zsh completion file must be copied to one of the folders under `$FPATH` variable.


## Documentation for API Endpoints

All URIs are relative to */v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*petApi* | [**addPet**](petApi.md#addpet) | **POST** /pet | Add a new pet to the store
*petApi* | [**updatePet**](petApi.md#updatepet) | **PUT** /pet | Update an existing pet
*petApi* | [**findPetsByStatus**](petApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*petApi* | [**findPetsByTags**](petApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*petApi* | [**getPetById**](petApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*petApi* | [**updatePetWithForm**](petApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*petApi* | [**deletePet**](petApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*petApi* | [**uploadFile**](petApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*storeApi* | [**getInventory**](storeApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*storeApi* | [**placeOrder**](storeApi.md#placeorder) | **POST** /store/order | Place an order for a pet
*storeApi* | [**getOrderById**](storeApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
*storeApi* | [**deleteOrder**](storeApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*userApi* | [**createUser**](userApi.md#createuser) | **POST** /user | Create user
*userApi* | [**createUsersWithArrayInput**](userApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
*userApi* | [**createUsersWithListInput**](userApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
*userApi* | [**loginUser**](userApi.md#loginuser) | **GET** /user/login | Logs user into the system
*userApi* | [**logoutUser**](userApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
*userApi* | [**getUserByName**](userApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
*userApi* | [**updateUser**](userApi.md#updateuser) | **PUT** /user/{username} | Updated user
*userApi* | [**deleteUser**](userApi.md#deleteuser) | **DELETE** /user/{username} | Delete user

## Documentation For Models

 - [Order](Order.md)
 - [Category](Category.md)
 - [User](User.md)
 - [Tag](Tag.md)
 - [Pet](Pet.md)
 - [ApiResponse](ApiResponse.md)

## Documentation For Authorization


## petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/oauth/dialog
- **Scopes**:
  - **write:pets**: 
  - **read:pets**: 

## api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

