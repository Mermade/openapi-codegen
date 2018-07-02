# IO.OpenAPI\userApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](userApi.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](userApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](userApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**loginUser**](userApi.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](userApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**getUserByName**](userApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**updateUser**](userApi.md#updateUser) | **PUT** /user/{username} | Updated user
[**deleteUser**](userApi.md#deleteUser) | **DELETE** /user/{username} | Delete user

# **createUser**
> createUser(body)
Create user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**struct{}**](object.md)| Created user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)
Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**[100]struct{}**](object.md)| List of user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
> createUsersWithListInput(body)
Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**[100]struct{}**](object.md)| List of user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
> string loginUser(username, password)
Logs user into the system



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The user name for login | 
  **password** | **string**| The password for login in clear text | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
> logoutUser()
Logs out current logged in user session



### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
> User getUserByName(username)
Get user by user name



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**User**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
> updateUser(username, body)
Updated user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| name that need to be updated | 
  **body** | [**struct{}**](object.md)| Updated user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
> deleteUser(username)
Delete user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The name that needs to be deleted | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

