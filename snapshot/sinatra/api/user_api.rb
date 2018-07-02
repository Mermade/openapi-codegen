require 'json'


MyApp.add_route('POST', '/v2/user', {
  "resourcePath" => "/user",
  "summary" => "Create user",
  "nickname" => "createUser", 
  "responseClass" => "void", 
  "endpoint" => "/user", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Created user object",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/user/createWithArray', {
  "resourcePath" => "/user",
  "summary" => "Creates list of users with given input array",
  "nickname" => "createUsersWithArrayInput", 
  "responseClass" => "void", 
  "endpoint" => "/user/createWithArray", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "[100]struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/user/createWithList', {
  "resourcePath" => "/user",
  "summary" => "Creates list of users with given input array",
  "nickname" => "createUsersWithListInput", 
  "responseClass" => "void", 
  "endpoint" => "/user/createWithList", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "[100]struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/login', {
  "resourcePath" => "/user",
  "summary" => "Logs user into the system",
  "nickname" => "loginUser", 
  "responseClass" => "string", 
  "endpoint" => "/user/login", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The user name for login",
      "dataType" => "string",
      "paramType" => "query",
      
      "allowableValues" => "",
      
    },
    {
      "name" => "password",
      "description" => "The password for login in clear text",
      "dataType" => "string",
      "paramType" => "query",
      
      "allowableValues" => "",
      
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/logout', {
  "resourcePath" => "/user",
  "summary" => "Logs out current logged in user session",
  "nickname" => "logoutUser", 
  "responseClass" => "void", 
  "endpoint" => "/user/logout", 
  "notes" => "",
  "parameters" => [
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/{username}', {
  "resourcePath" => "/user",
  "summary" => "Get user by user name",
  "nickname" => "getUserByName", 
  "responseClass" => "User", 
  "endpoint" => "/user/{username}", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be fetched. Use user1 for testing. ",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('PUT', '/v2/user/{username}', {
  "resourcePath" => "/user",
  "summary" => "Updated user",
  "nickname" => "updateUser", 
  "responseClass" => "void", 
  "endpoint" => "/user/{username}", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "name that need to be updated",
      "dataType" => "string",
      "paramType" => "path",
    },
    {
      "name" => "body",
      "description" => "Updated user object",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('DELETE', '/v2/user/{username}', {
  "resourcePath" => "/user",
  "summary" => "Delete user",
  "nickname" => "deleteUser", 
  "responseClass" => "void", 
  "endpoint" => "/user/{username}", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be deleted",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

