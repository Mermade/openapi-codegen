# IO.OpenAPI\petApi

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
> addPet(ctx, body)
Add a new pet to the store



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **body** | [**struct{}**](object.md)| Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
> updatePet(ctx, body)
Update an existing pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **body** | [**struct{}**](object.md)| Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
> [100]struct{} findPetsByStatus(ctx, status)
Finds Pets by status

Multiple status values can be provided with comma separated strings

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
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
> [100]struct{} findPetsByTags(ctx, tags)
Finds Pets by tags

Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
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
> Pet getPetById(ctx, petId)
Find pet by ID

Returns a single pet

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
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
> updatePetWithForm(ctx, petId, optional)
Updates a pet in the store with form data



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **petId** | **int**| ID of pet that needs to be updated | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet that needs to be updated | 
 **body** | [**struct{}**](object.md)|  | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
> deletePet(ctx, petId, optional)
Deletes a pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **petId** | **int**| Pet id to delete | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| Pet id to delete | 
 **api_key** | **string**|  | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
> ApiResponse uploadFile(ctx, petId, optional)
uploads an image



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **petId** | **int**| ID of pet to update | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to update | 
 **body** | [**string**](object.md)|  | 

### Return type

[**ApiResponse**](object.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

