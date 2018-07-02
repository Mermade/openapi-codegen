# IO.OpenAPI\storeApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getInventory**](storeApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**placeOrder**](storeApi.md#placeOrder) | **POST** /store/order | Place an order for a pet
[**getOrderById**](storeApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**deleteOrder**](storeApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID

# **getInventory**
> struct{} getInventory(ctx, )
Returns pet inventories by status

Returns a map of status codes to quantities

### Required Parameters
This endpoint does not need any parameter.

### Return type

**struct{}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
> Order placeOrder(body)
Place an order for a pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**struct{}**](object.md)| order placed for purchasing the pet | 

### Return type

[**Order**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
> Order getOrderById(orderId)
Find purchase order by ID

For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **orderId** | **int**| ID of pet that needs to be fetched | 

### Return type

[**Order**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteOrder**
> deleteOrder(orderId)
Delete purchase order by ID

For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **orderId** | **int**| ID of the order that needs to be deleted | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

