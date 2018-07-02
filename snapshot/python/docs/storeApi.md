# swagger_client.storeApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getInventory**](storeApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**placeOrder**](storeApi.md#placeOrder) | **POST** /store/order | Place an order for a pet
[**getOrderById**](storeApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**deleteOrder**](storeApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID

# **getInventory**
> struct{} getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

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
api_instance = swagger_client.storeApi(swagger_client.ApiClient(configuration))

try:
    # 
    api_response = api_instance.getInventory()
    pprint(api_response)
except ApiException as e:
    print("Exception when calling storeApi->getInventory: %s\n" % e)
```

### Parameters
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



### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.storeApi(swagger_client.ApiClient(configuration))
body = {"id":0,"petId":0,"quantity":0,"shipDate":"2018-07-02T10:01:52Z","status":"placed","complete":false} # struct{} | order placed for purchasing the pet

try:
    # 
    api_response = api_instance.placeOrder(body)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling storeApi->placeOrder: %s\n" % e)
```

### Parameters

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

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.storeApi(swagger_client.ApiClient(configuration))
orderId = 1 # int | ID of pet that needs to be fetched

try:
    # 
    api_response = api_instance.getOrderById(orderId)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling storeApi->getOrderById: %s\n" % e)
```

### Parameters

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

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint


# create an instance of the API class
api_instance = swagger_client.storeApi(swagger_client.ApiClient(configuration))
orderId = 1 # int | ID of the order that needs to be deleted

try:
    # 
    api_instance.deleteOrder(orderId)
except ApiException as e:
    print("Exception when calling storeApi->deleteOrder: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **int**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/x-www-form-urlencoded, application/octet-stream
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

