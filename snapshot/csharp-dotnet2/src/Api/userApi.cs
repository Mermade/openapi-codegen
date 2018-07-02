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
    public interface IuserApi
    {
        /// <summary>
        /// Create user This can only be done by the logged in user.
        /// </summary>
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        void createUser (object body);
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void createUsersWithArrayInput (array body);
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void createUsersWithListInput (array body);
        /// <summary>
        /// Logs user into the system 
        /// </summary>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        string loginUser (string username, string password);
        /// <summary>
        /// Logs out current logged in user session 
        /// </summary>
        /// <returns></returns>
        void logoutUser ();
        /// <summary>
        /// Get user by user name 
        /// </summary>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing. </param>
        /// <returns>User</returns>
        User getUserByName (string username);
        /// <summary>
        /// Updated user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">name that need to be updated</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        void updateUser (string username, object body);
        /// <summary>
        /// Delete user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        void deleteUser (string username);
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class userApi : IuserApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="userApi"/> class.
        /// </summary>
        /// <param name="apiClient"> an instance of ApiClient (optional)</param>
        /// <returns></returns>
        public userApi(ApiClient apiClient = null)
        {
            if (apiClient == null) // use the default one in Configuration
                this.ApiClient = Configuration.DefaultApiClient; 
            else
                this.ApiClient = apiClient;
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="userApi"/> class.
        /// </summary>
        /// <returns></returns>
        public userApi(String basePath)
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
        /// Create user This can only be done by the logged in user.
        /// </summary>
        /// <param name="body">Created user object</param> 
        /// <returns></returns>            
        public void createUser (object body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling createUser");
    
            var path = "/user";
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
                throw new ApiException ((int)response.StatusCode, "Error calling createUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling createUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param> 
        /// <returns></returns>            
        public void createUsersWithArrayInput (array body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling createUsersWithArrayInput");
    
            var path = "/user/createWithArray";
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
                throw new ApiException ((int)response.StatusCode, "Error calling createUsersWithArrayInput: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling createUsersWithArrayInput: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Creates list of users with given input array 
        /// </summary>
        /// <param name="body">List of user object</param> 
        /// <returns></returns>            
        public void createUsersWithListInput (array body)
        {
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling createUsersWithListInput");
    
            var path = "/user/createWithList";
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
                throw new ApiException ((int)response.StatusCode, "Error calling createUsersWithListInput: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling createUsersWithListInput: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Logs user into the system 
        /// </summary>
        /// <param name="username">The user name for login</param> 
        /// <param name="password">The password for login in clear text</param> 
        /// <returns>string</returns>            
        public string loginUser (string username, string password)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling loginUser");
            // verify the required parameter 'password' is set
            if (password == null) throw new ApiException(400, "Missing required parameter 'password' when calling loginUser");
    
            var path = "/user/login";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
             if (username != null) queryParams.Add("username", ApiClient.ParameterToString(username)); // query parameter
 if (password != null) queryParams.Add("password", ApiClient.ParameterToString(password)); // query parameter
                                        
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling loginUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling loginUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return (string) ApiClient.Deserialize(response.Content, typeof(string), response.Headers);
        }
    
        /// <summary>
        /// Logs out current logged in user session 
        /// </summary>
        /// <returns></returns>            
        public void logoutUser ()
        {
    
            var path = "/user/logout";
            path = path.Replace("{format}", "json");
                
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
                throw new ApiException ((int)response.StatusCode, "Error calling logoutUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling logoutUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Get user by user name 
        /// </summary>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing. </param> 
        /// <returns>User</returns>            
        public User getUserByName (string username)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling getUserByName");
    
            var path = "/user/{username}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "username" + "}", ApiClient.ParameterToString(username));
    
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
                throw new ApiException ((int)response.StatusCode, "Error calling getUserByName: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling getUserByName: " + response.ErrorMessage, response.ErrorMessage);
    
            return (User) ApiClient.Deserialize(response.Content, typeof(User), response.Headers);
        }
    
        /// <summary>
        /// Updated user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">name that need to be updated</param> 
        /// <param name="body">Updated user object</param> 
        /// <returns></returns>            
        public void updateUser (string username, object body)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling updateUser");
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling updateUser");
    
            var path = "/user/{username}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "username" + "}", ApiClient.ParameterToString(username));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling updateUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling updateUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Delete user This can only be done by the logged in user.
        /// </summary>
        /// <param name="username">The name that needs to be deleted</param> 
        /// <returns></returns>            
        public void deleteUser (string username)
        {
            // verify the required parameter 'username' is set
            if (username == null) throw new ApiException(400, "Missing required parameter 'username' when calling deleteUser");
    
            var path = "/user/{username}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "username" + "}", ApiClient.ParameterToString(username));
    
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
                throw new ApiException ((int)response.StatusCode, "Error calling deleteUser: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling deleteUser: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
    }
}
