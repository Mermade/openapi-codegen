# IO.OpenAPI.storeApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getInventory**](storeApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**placeOrder**](storeApi.md#placeorder) | **POST** /store/order | Place an order for a pet
[**getOrderById**](storeApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**deleteOrder**](storeApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID

<a name="getinventory"></a>
# **getInventory**
> object getInventory ()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class getInventoryExample
    {
        public void main()
        {

            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new storeApi();

            try
            {
                // 
                object result = apiInstance.getInventory();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling storeApi.getInventory: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**object**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="placeorder"></a>
# **placeOrder**
> Order placeOrder (object body)

Place an order for a pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class placeOrderExample
    {
        public void main()
        {


            var apiInstance = new storeApi();
            var body = new object(); // object | order placed for purchasing the pet

            try
            {
                // 
                Order result = apiInstance.placeOrder(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling storeApi.placeOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**object**](object.md)| order placed for purchasing the pet | 

### Return type

[**Order**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getorderbyid"></a>
# **getOrderById**
> Order getOrderById (integer orderId)

Find purchase order by ID

For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class getOrderByIdExample
    {
        public void main()
        {


            var apiInstance = new storeApi();
            var orderId = 1;  // integer | ID of pet that needs to be fetched

            try
            {
                // 
                Order result = apiInstance.getOrderById(orderId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling storeApi.getOrderById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **integer**| ID of pet that needs to be fetched | 

### Return type

[**Order**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deleteorder"></a>
# **deleteOrder**
> void deleteOrder (integer orderId)

Delete purchase order by ID

For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors

### Example
```csharp
using System;
using System.Diagnostics;
using IO.OpenAPI;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace Example
{
    public class deleteOrderExample
    {
        public void main()
        {


            var apiInstance = new storeApi();
            var orderId = 1;  // integer | ID of the order that needs to be deleted

            try
            {
                // 
                apiInstance.deleteOrder(orderId);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling storeApi.deleteOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **integer**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

