# IO.OpenAPI.userApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](userApi.md#createuser) | **POST** /user | Create user
[**createUsersWithArrayInput**](userApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](userApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**loginUser**](userApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**logoutUser**](userApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**getUserByName**](userApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**updateUser**](userApi.md#updateuser) | **PUT** /user/{username} | Updated user
[**deleteUser**](userApi.md#deleteuser) | **DELETE** /user/{username} | Delete user

<a name="createuser"></a>
# **createUser**
> void createUser (object body)

Create user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class createUserExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var body = new object(); // object | Created user object

            try
            {
                // 
                apiInstance.createUser(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.createUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**object**](object.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswitharrayinput"></a>
# **createUsersWithArrayInput**
> void createUsersWithArrayInput (array body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class createUsersWithArrayInputExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var body = new array(); // array | List of user object

            try
            {
                // 
                apiInstance.createUsersWithArrayInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.createUsersWithArrayInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**array**](object.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswithlistinput"></a>
# **createUsersWithListInput**
> void createUsersWithListInput (array body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class createUsersWithListInputExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var body = new array(); // array | List of user object

            try
            {
                // 
                apiInstance.createUsersWithListInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.createUsersWithListInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**array**](object.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="loginuser"></a>
# **loginUser**
> string loginUser (string username, string password)

Logs user into the system



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class loginUserExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var username = "string";  // string | The user name for login
            var password = "pa$$word";  // string | The password for login in clear text

            try
            {
                // 
                string result = apiInstance.loginUser(username, password);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.loginUser: " + e.Message );
            }
        }
    }
}
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

<a name="logoutuser"></a>
# **logoutUser**
> void logoutUser ()

Logs out current logged in user session



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class logoutUserExample
    {
        public void main()
        {


            var apiInstance = new userApi();

            try
            {
                // 
                apiInstance.logoutUser();
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.logoutUser: " + e.Message );
            }
        }
    }
}
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

<a name="getuserbyname"></a>
# **getUserByName**
> User getUserByName (string username)

Get user by user name



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class getUserByNameExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var username = "string";  // string | The name that needs to be fetched. Use user1 for testing. 

            try
            {
                // 
                User result = apiInstance.getUserByName(username);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.getUserByName: " + e.Message );
            }
        }
    }
}
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

<a name="updateuser"></a>
# **updateUser**
> void updateUser (string username, object body)

Updated user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class updateUserExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var username = "string";  // string | name that need to be updated
            var body = new object(); // object | Updated user object

            try
            {
                // 
                apiInstance.updateUser(username, body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.updateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be updated | 
 **body** | [**object**](object.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deleteuser"></a>
# **deleteUser**
> void deleteUser (string username)

Delete user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class deleteUserExample
    {
        public void main()
        {


            var apiInstance = new userApi();
            var username = "string";  // string | The name that needs to be deleted

            try
            {
                // 
                apiInstance.deleteUser(username);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling userApi.deleteUser: " + e.Message );
            }
        }
    }
}
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

