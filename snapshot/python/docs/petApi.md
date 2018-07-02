# swagger_client.petApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](petApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**updatePet**](petApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**findPetsByStatus**](petApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](petApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](petApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePetWithForm**](petApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**deletePet**](petApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**uploadFile**](petApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image

# **addPet**
> addPet(body)

Add a new pet to the store



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
body = {"id":0,"category":{"id":0,"name":"string"},"name":"doggie","photoUrls":["string"],"tags":[{"id":0,"name":"string"}],"status":"available"} # struct{} | Pet object that needs to be added to the store

try:
    # 
    api_instance.addPet(body)
except ApiException as e:
    print("Exception when calling petApi->addPet: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**struct{}**](object.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
> updatePet(body)

Update an existing pet



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
body = {"id":0,"category":{"id":0,"name":"string"},"name":"doggie","photoUrls":["string"],"tags":[{"id":0,"name":"string"}],"status":"available"} # struct{} | Pet object that needs to be added to the store

try:
    # 
    api_instance.updatePet(body)
except ApiException as e:
    print("Exception when calling petApi->updatePet: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**struct{}**](object.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
> [100]struct{} findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
status = ["available"] # [100]string | Status values that need to be considered for filter

try:
    # 
    api_response = api_instance.findPetsByStatus(status)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling petApi->findPetsByStatus: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **[100]string**| Status values that need to be considered for filter | 

### Return type

**[100]struct{}**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
> [100]struct{} findPetsByTags(tags)

Finds Pets by tags

Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
tags = ["string"] # [100]string | Tags to filter by

try:
    # 
    api_response = api_instance.findPetsByTags(tags)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling petApi->findPetsByTags: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | **[100]string**| Tags to filter by | 

### Return type

**[100]struct{}**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure API key authorization: api_key
configuration = swagger_client.Configuration()
configuration.api_key['api_key'] = 'YOUR_API_KEY'
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key'] = 'Bearer'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
petId = 0 # int | ID of pet to return

try:
    # 
    api_response = api_instance.getPetById(petId)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling petApi->getPetById: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to return | 

### Return type

[**Pet**](object.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
> updatePetWithForm(petId, body=body)

Updates a pet in the store with form data



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
petId = 0 # int | ID of pet that needs to be updated
body = {"name":"string","status":"string"} # struct{} |  (optional)

try:
    # 
    api_instance.updatePetWithForm(petId, body=body)
except ApiException as e:
    print("Exception when calling petApi->updatePetWithForm: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet that needs to be updated | 
 **body** | [**struct{}**](object.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
> deletePet(petId, api_key=api_key)

Deletes a pet



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
petId = 0 # int | Pet id to delete
api_key = "string" # string |  (optional)

try:
    # 
    api_instance.deletePet(petId, api_key=api_key)
except ApiException as e:
    print("Exception when calling petApi->deletePet: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| Pet id to delete | 
 **api_key** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
> ApiResponse uploadFile(petId, body=body)

uploads an image



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure OAuth2 access token for authorization: petstore_auth
configuration = swagger_client.Configuration()
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.petApi(swagger_client.ApiClient(configuration))
petId = 0 # int | ID of pet to update
body = "string" # string |  (optional)

try:
    # 
    api_response = api_instance.uploadFile(petId, body=body)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling petApi->uploadFile: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to update | 
 **body** | [**string**](object.md)|  | [optional] 

### Return type

[**ApiResponse**](object.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

