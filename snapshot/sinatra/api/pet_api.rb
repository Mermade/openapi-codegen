require 'json'


MyApp.add_route('POST', '/v2/pet', {
  "resourcePath" => "/pet",
  "summary" => "Add a new pet to the store",
  "nickname" => "addPet", 
  "responseClass" => "void", 
  "endpoint" => "/pet", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Pet object that needs to be added to the store",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('PUT', '/v2/pet', {
  "resourcePath" => "/pet",
  "summary" => "Update an existing pet",
  "nickname" => "updatePet", 
  "responseClass" => "void", 
  "endpoint" => "/pet", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Pet object that needs to be added to the store",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pet/findByStatus', {
  "resourcePath" => "/pet",
  "summary" => "Finds Pets by status",
  "nickname" => "findPetsByStatus", 
  "responseClass" => "[100]struct{}", 
  "endpoint" => "/pet/findByStatus", 
  "notes" => "Multiple status values can be provided with comma separated strings",
  "parameters" => [
    {
      "name" => "status",
      "description" => "Status values that need to be considered for filter",
      "dataType" => "[100]string",
      "paramType" => "query",
      
      "allowableValues" => "",
      
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pet/findByTags', {
  "resourcePath" => "/pet",
  "summary" => "Finds Pets by tags",
  "nickname" => "findPetsByTags", 
  "responseClass" => "[100]struct{}", 
  "endpoint" => "/pet/findByTags", 
  "notes" => "Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.",
  "parameters" => [
    {
      "name" => "tags",
      "description" => "Tags to filter by",
      "dataType" => "[100]string",
      "paramType" => "query",
      
      "allowableValues" => "",
      
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pet/{petId}', {
  "resourcePath" => "/pet",
  "summary" => "Find pet by ID",
  "nickname" => "getPetById", 
  "responseClass" => "Pet", 
  "endpoint" => "/pet/{petId}", 
  "notes" => "Returns a single pet",
  "parameters" => [
    {
      "name" => "petId",
      "description" => "ID of pet to return",
      "dataType" => "int",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/pet/{petId}', {
  "resourcePath" => "/pet",
  "summary" => "Updates a pet in the store with form data",
  "nickname" => "updatePetWithForm", 
  "responseClass" => "void", 
  "endpoint" => "/pet/{petId}", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "petId",
      "description" => "ID of pet that needs to be updated",
      "dataType" => "int",
      "paramType" => "path",
    },
    {
      "name" => "body",
      "description" => "",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('DELETE', '/v2/pet/{petId}', {
  "resourcePath" => "/pet",
  "summary" => "Deletes a pet",
  "nickname" => "deletePet", 
  "responseClass" => "void", 
  "endpoint" => "/pet/{petId}", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "petId",
      "description" => "Pet id to delete",
      "dataType" => "int",
      "paramType" => "path",
    },
    {
      "name" => "api_key",
      "description" => "",
      "dataType" => "string",
      "paramType" => "header",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/pet/{petId}/uploadImage', {
  "resourcePath" => "/pet",
  "summary" => "uploads an image",
  "nickname" => "uploadFile", 
  "responseClass" => "ApiResponse", 
  "endpoint" => "/pet/{petId}/uploadImage", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "petId",
      "description" => "ID of pet to update",
      "dataType" => "int",
      "paramType" => "path",
    },
    {
      "name" => "body",
      "description" => "",
      "dataType" => "string",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

