require 'json'


MyApp.add_route('GET', '/v2/store/inventory', {
  "resourcePath" => "/store",
  "summary" => "Returns pet inventories by status",
  "nickname" => "getInventory", 
  "responseClass" => "struct{}", 
  "endpoint" => "/store/inventory", 
  "notes" => "Returns a map of status codes to quantities",
  "parameters" => [
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/store/order', {
  "resourcePath" => "/store",
  "summary" => "Place an order for a pet",
  "nickname" => "placeOrder", 
  "responseClass" => "Order", 
  "endpoint" => "/store/order", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "order placed for purchasing the pet",
      "dataType" => "struct{}",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/store/order/{orderId}', {
  "resourcePath" => "/store",
  "summary" => "Find purchase order by ID",
  "nickname" => "getOrderById", 
  "responseClass" => "Order", 
  "endpoint" => "/store/order/{orderId}", 
  "notes" => "For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions",
  "parameters" => [
    {
      "name" => "orderId",
      "description" => "ID of pet that needs to be fetched",
      "dataType" => "int",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('DELETE', '/v2/store/order/{orderId}', {
  "resourcePath" => "/store",
  "summary" => "Delete purchase order by ID",
  "nickname" => "deleteOrder", 
  "responseClass" => "void", 
  "endpoint" => "/store/order/{orderId}", 
  "notes" => "For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors",
  "parameters" => [
    {
      "name" => "orderId",
      "description" => "ID of the order that needs to be deleted",
      "dataType" => "int",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

