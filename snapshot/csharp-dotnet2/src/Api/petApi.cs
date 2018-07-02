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
    public interface IpetApi
    {
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void addPet (object body);
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void updatePet (object body);
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma separated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>array</returns>
        array findPetsByStatus (array status);
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>array</returns>
        array findPetsByTags (array tags);
        /// <summary>
        /// Find pet by ID Returns a single pet
        /// </summary>
        /// <param name="petId">ID of pet to return</param>
        /// <returns>Pet</returns>
        Pet getPetById (integer petId);
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="body"></param>
        /// <returns></returns>
        void updatePetWithForm (integer petId, object body);
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="api_key"></param>
        /// <returns></returns>
        void deletePet (integer petId, string api_key);
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="body"></param>
        /// <returns>ApiResponse</returns>
        ApiResponse uploadFile (integer petId, string body);
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class petApi : IpetApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="petApi"/> class.
        /// </summary>
        /// <param name="apiClient"> an instance of ApiClient (optional)</param>
        /// <returns></returns>
        public petApi(ApiClient apiClient = null)
        {
            if (apiClient == null) // use the default one in Configuration
                this.ApiClient = Configuration.DefaultApiClient; 
            else
                this.ApiClient = apiClient;
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="petApi"/> class.
        /// </summary>
        /// <returns></returns>
        public petApi(String basePath)
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
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void addPet (object body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling addPet");
    
            var path = "/pet";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling addPet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling addPet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void updatePet (object body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling updatePet");
    
            var path = "/pet";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling updatePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling updatePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma separated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param> 
        /// <returns>array</returns>            
        public array findPetsByStatus (array status)
        {
            // verify the required parameter 'status' is set
            if (status == null) throw new ApiException(400, "Missing required parameter 'status' when calling findPetsByStatus");
    
            var path = "/pet/findByStatus";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
             if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
                                        
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling findPetsByStatus: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling findPetsByStatus: " + response.ErrorMessage, response.ErrorMessage);
    
            return (array) ApiClient.Deserialize(response.Content, typeof(array), response.Headers);
        }
    
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param> 
        /// <returns>array</returns>            
        public array findPetsByTags (array tags)
        {
            // verify the required parameter 'tags' is set
            if (tags == null) throw new ApiException(400, "Missing required parameter 'tags' when calling findPetsByTags");
    
            var path = "/pet/findByTags";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
             if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
                                        
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling findPetsByTags: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling findPetsByTags: " + response.ErrorMessage, response.ErrorMessage);
    
            return (array) ApiClient.Deserialize(response.Content, typeof(array), response.Headers);
        }
    
        /// <summary>
        /// Find pet by ID Returns a single pet
        /// </summary>
        /// <param name="petId">ID of pet to return</param> 
        /// <returns>Pet</returns>            
        public Pet getPetById (integer petId)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling getPetById");
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
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
                throw new ApiException ((int)response.StatusCode, "Error calling getPetById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling getPetById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Pet) ApiClient.Deserialize(response.Content, typeof(Pet), response.Headers);
        }
    
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param> 
        /// <param name="body"></param> 
        /// <returns></returns>            
        public void updatePetWithForm (integer petId, object body)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling updatePetWithForm");
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling updatePetWithForm: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling updatePetWithForm: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param> 
        /// <param name="api_key"></param> 
        /// <returns></returns>            
        public void deletePet (integer petId, string api_key)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling deletePet");
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                         if (api_key != null) headerParams.Add("api_key", ApiClient.ParameterToString(api_key)); // header parameter
                            
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling deletePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling deletePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param> 
        /// <param name="body"></param> 
        /// <returns>ApiResponse</returns>            
        public ApiResponse uploadFile (integer petId, string body)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling uploadFile");
    
            var path = "/pet/{petId}/uploadImage";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling uploadFile: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling uploadFile: " + response.ErrorMessage, response.ErrorMessage);
    
            return (ApiResponse) ApiClient.Deserialize(response.Content, typeof(ApiResponse), response.Headers);
        }
    
    }
}
