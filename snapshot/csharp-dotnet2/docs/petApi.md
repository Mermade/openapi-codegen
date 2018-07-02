# IO.OpenAPI.petApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](petApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**updatePet**](petApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**findPetsByStatus**](petApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](petApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](petApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**updatePetWithForm**](petApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**deletePet**](petApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**uploadFile**](petApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image

<a name="addpet"></a>
# **addPet**
> void addPet (object body)

Add a new pet to the store



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class addPetExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var body = new object(); // object | Pet object that needs to be added to the store

            try
            {
                // 
                apiInstance.addPet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.addPet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**object**](object.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepet"></a>
# **updatePet**
> void updatePet (object body)

Update an existing pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class updatePetExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var body = new object(); // object | Pet object that needs to be added to the store

            try
            {
                // 
                apiInstance.updatePet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.updatePet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**object**](object.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbystatus"></a>
# **findPetsByStatus**
> array findPetsByStatus (array status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class findPetsByStatusExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var status = ["available"];  // array | Status values that need to be considered for filter

            try
            {
                // 
                array result = apiInstance.findPetsByStatus(status);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.findPetsByStatus: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **array**| Status values that need to be considered for filter | 

### Return type

**array**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbytags"></a>
# **findPetsByTags**
> array findPetsByTags (array tags)

Finds Pets by tags

Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class findPetsByTagsExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var tags = ["string"];  // array | Tags to filter by

            try
            {
                // 
                array result = apiInstance.findPetsByTags(tags);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.findPetsByTags: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | **array**| Tags to filter by | 

### Return type

**array**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getpetbyid"></a>
# **getPetById**
> Pet getPetById (integer petId)

Find pet by ID

Returns a single pet

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class getPetByIdExample
    {
        public void main()
        {

            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new petApi();
            var petId = 0;  // integer | ID of pet to return

            try
            {
                // 
                Pet result = apiInstance.getPetById(petId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.getPetById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **integer**| ID of pet to return | 

### Return type

[**Pet**](object.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepetwithform"></a>
# **updatePetWithForm**
> void updatePetWithForm (integer petId, object body)

Updates a pet in the store with form data



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class updatePetWithFormExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var petId = 0;  // integer | ID of pet that needs to be updated
            var body = new object(); // object |  (optional) 

            try
            {
                // 
                apiInstance.updatePetWithForm(petId, body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.updatePetWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **integer**| ID of pet that needs to be updated | 
 **body** | [**object**](object.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deletepet"></a>
# **deletePet**
> void deletePet (integer petId, string api_key)

Deletes a pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class deletePetExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var petId = 0;  // integer | Pet id to delete
            var api_key = "string";  // string |  (optional) 

            try
            {
                // 
                apiInstance.deletePet(petId, api_key);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.deletePet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **integer**| Pet id to delete | 
 **api_key** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="uploadfile"></a>
# **uploadFile**
> ApiResponse uploadFile (integer petId, string body)

uploads an image



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class uploadFileExample
    {
        public void main()
        {

            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new petApi();
            var petId = 0;  // integer | ID of pet to update
            var body = new string(); // string |  (optional) 

            try
            {
                // 
                ApiResponse result = apiInstance.uploadFile(petId, body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling petApi.uploadFile: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **integer**| ID of pet to update | 
 **body** | [**string**](object.md)|  | [optional] 

### Return type

[**ApiResponse**](object.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

