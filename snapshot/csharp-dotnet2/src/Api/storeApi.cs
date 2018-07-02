using System;
using System.Collections.Generic;
using RestSharp;
using IO.OpenAPI.Client;
using IO.OpenAPI;

namespace IO.OpenAPI
{
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IstoreApi
    {
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns>object</returns>
        object getInventory ();
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        Order placeOrder (object body);
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &gt;= 1 and &lt;= 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        Order getOrderById (integer orderId);
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        void deleteOrder (integer orderId);
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class storeApi : IstoreApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="storeApi"/> class.
        /// </summary>
        /// <param name="apiClient"> an instance of ApiClient (optional)</param>
        /// <returns></returns>
        public storeApi(ApiClient apiClient = null)
        {
            if (apiClient == null) // use the default one in Configuration
                this.ApiClient = Configuration.DefaultApiClient; 
            else
                this.ApiClient = apiClient;
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="storeApi"/> class.
        /// </summary>
        /// <returns></returns>
        public storeApi(String basePath)
        {
            this.ApiClient = new ApiClient(basePath);
        }
    
        /// <summary>
        /// Sets the base path of the API client.
        /// </summary>
        /// <param name="basePath">The base path</param>
        /// <value>The base path</value>
        public void SetBasePath(String basePath)
        {
            this.ApiClient.BasePath = basePath;
        }
    
        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <param name="basePath">The base path</param>
        /// <value>The base path</value>
        public String GetBasePath(String basePath)
        {
            return this.ApiClient.BasePath;
        }
    
        /// <summary>
        /// Gets or sets the API client.
        /// </summary>
        /// <value>An instance of the ApiClient</value>
        public ApiClient ApiClient {get; set;}
    
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns>object</returns>            
        public object getInventory ()
        {
    
            var path = "/store/inventory";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling getInventory: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling getInventory: " + response.ErrorMessage, response.ErrorMessage);
    
            return (object) ApiClient.Deserialize(response.Content, typeof(object), response.Headers);
        }
    
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param> 
        /// <returns>Order</returns>            
        public Order placeOrder (object body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling placeOrder");
    
            var path = "/store/order";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling placeOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling placeOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response.Content, typeof(Order), response.Headers);
        }
    
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &gt;= 1 and &lt;= 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param> 
        /// <returns>Order</returns>            
        public Order getOrderById (integer orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling getOrderById");
    
            var path = "/store/order/{orderId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "orderId" + "}", ApiClient.ParameterToString(orderId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling getOrderById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling getOrderById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response.Content, typeof(Order), response.Headers);
        }
    
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param> 
        /// <returns></returns>            
        public void deleteOrder (integer orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling deleteOrder");
    
            var path = "/store/order/{orderId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "orderId" + "}", ApiClient.ParameterToString(orderId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling deleteOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling deleteOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
    }
}
