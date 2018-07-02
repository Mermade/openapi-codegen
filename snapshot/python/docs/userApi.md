# swagger_client.userApi

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

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
body = {"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0} # struct{} | Created user object

try:
    # 
    api_instance.createUser(body)
except ApiException as e:
    print("Exception when calling userApi->createUser: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**struct{}**](object.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)

Creates list of users with given input array



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
body = [{"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}] # [100]struct{} | List of user object

try:
    # 
    api_instance.createUsersWithArrayInput(body)
except ApiException as e:
    print("Exception when calling userApi->createUsersWithArrayInput: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[100]struct{}**](object.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
> createUsersWithListInput(body)

Creates list of users with given input array



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
body = [{"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}] # [100]struct{} | List of user object

try:
    # 
    api_instance.createUsersWithListInput(body)
except ApiException as e:
    print("Exception when calling userApi->createUsersWithListInput: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[100]struct{}**](object.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
> string loginUser(username, password)

Logs user into the system



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
username = "string" # string | The user name for login
password = "pa$$word" # string | The password for login in clear text

try:
    # 
    api_response = api_instance.loginUser(username, password)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling userApi->loginUser: %s\n" % e)
```

### Parameters

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



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))

try:
    # 
    api_instance.logoutUser()
except ApiException as e:
    print("Exception when calling userApi->logoutUser: %s\n" % e)
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
> User getUserByName(username)

Get user by user name



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
username = "string" # string | The name that needs to be fetched. Use user1 for testing. 

try:
    # 
    api_response = api_instance.getUserByName(username)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling userApi->getUserByName: %s\n" % e)
```

### Parameters

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

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
username = "string" # string | name that need to be updated
body = {"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0} # struct{} | Updated user object

try:
    # 
    api_instance.updateUser(username, body)
except ApiException as e:
    print("Exception when calling userApi->updateUser: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be updated | 
 **body** | [**struct{}**](object.md)| Updated user object | 

### Return type

void (empty response body)

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

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.userApi(swagger_client.ApiClient(configuration))
username = "string" # string | The name that needs to be deleted

try:
    # 
    api_instance.deleteUser(username)
except ApiException as e:
    print("Exception when calling userApi->deleteUser: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

