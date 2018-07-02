# Order

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Swagger Petstore | [optional] 
**petId** | **integer** | Swagger Petstore | [optional] 
**quantity** | **integer** | Swagger Petstore | [optional] 
**shipDate** | **string** | Swagger Petstore | [optional] 
**status** | **string** | Swagger Petstore | [optional] 
**complete** | **boolean** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

# Category

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Swagger Petstore | [optional] 
**name** | **string** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

# User

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Swagger Petstore | [optional] 
**username** | **string** | Swagger Petstore | [optional] 
**firstName** | **string** | Swagger Petstore | [optional] 
**lastName** | **string** | Swagger Petstore | [optional] 
**email** | **string** | Swagger Petstore | [optional] 
**password** | **string** | Swagger Petstore | [optional] 
**phone** | **string** | Swagger Petstore | [optional] 
**userStatus** | **integer** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

# Tag

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Swagger Petstore | [optional] 
**name** | **string** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

# Pet

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Swagger Petstore | [optional] 
**category** | [**object**](.md) | Swagger Petstore | [optional] 
**name** | **string** | Swagger Petstore | 
**photoUrls** | [**array**](.md) | Swagger Petstore | 
**tags** | [**array**](.md) | Swagger Petstore | [optional] 
**status** | **string** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

# ApiResponse

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**code** | **integer** | Swagger Petstore | [optional] 
**type** | **string** | Swagger Petstore | [optional] 
**message** | **string** | Swagger Petstore | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

