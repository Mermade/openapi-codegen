--  Swagger Petstore
--  This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.
--  ------------ EDIT NOTE ------------
--  This file was generated with swagger-codegen.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .swagger-codegen-ignore file:
--
--  src/IO.OpenAPI-servers.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with IO.OpenAPI.Model.Default;
with Swagger.Servers;
with IO.OpenAPI.Api.Models;
with IO.OpenAPI.Api.Skeletons;
package IO.OpenAPI.Api.Servers is
   use IO.OpenAPI.Api.Models;
   type Server_Type is limited new IO.OpenAPI.Api.Skeletons.Server_Type with null record;


   --  Add a new pet to the store
   overriding
   procedure addPet
      (Server : in out Server_Type;
       body : in object;
       Context : in out Swagger.Servers.Context_Type);

   --  Update an existing pet
   overriding
   procedure updatePet
      (Server : in out Server_Type;
       body : in object;
       Context : in out Swagger.Servers.Context_Type);

   --  Finds Pets by status
   overriding
   procedure findPetsByStatus
      (Server : in out Server_Type;
       status : in array;
       Result  : out array;
       Context : in out Swagger.Servers.Context_Type);

   --  Finds Pets by tags
   overriding
   procedure findPetsByTags
      (Server : in out Server_Type;
       tags : in array;
       Result  : out array;
       Context : in out Swagger.Servers.Context_Type);

   --  Find pet by ID
   overriding
   procedure getPetById
      (Server : in out Server_Type;
       petId : in integer;
       Result  : out Pet;
       Context : in out Swagger.Servers.Context_Type);

   --  Updates a pet in the store with form data
   overriding
   procedure updatePetWithForm
      (Server : in out Server_Type;
       petId : in integer;
       body : in object;
       Context : in out Swagger.Servers.Context_Type);

   --  Deletes a pet
   overriding
   procedure deletePet
      (Server : in out Server_Type;
       petId : in integer;
       api_key : in string;
       Context : in out Swagger.Servers.Context_Type);

   --  uploads an image
   overriding
   procedure uploadFile
      (Server : in out Server_Type;
       petId : in integer;
       body : in string;
       Result  : out ApiResponse;
       Context : in out Swagger.Servers.Context_Type);

   --  Returns pet inventories by status
   overriding
   procedure getInventory
      (Server : in out Server_Type
       ;
       Result  : out object;
       Context : in out Swagger.Servers.Context_Type);

   --  Place an order for a pet
   overriding
   procedure placeOrder
      (Server : in out Server_Type;
       body : in object;
       Result  : out Order;
       Context : in out Swagger.Servers.Context_Type);

   --  Find purchase order by ID
   overriding
   procedure getOrderById
      (Server : in out Server_Type;
       orderId : in integer;
       Result  : out Order;
       Context : in out Swagger.Servers.Context_Type);

   --  Delete purchase order by ID
   overriding
   procedure deleteOrder
      (Server : in out Server_Type;
       orderId : in integer;
       Context : in out Swagger.Servers.Context_Type);

   --  Create user
   overriding
   procedure createUser
      (Server : in out Server_Type;
       body : in object;
       Context : in out Swagger.Servers.Context_Type);

   --  Creates list of users with given input array
   overriding
   procedure createUsersWithArrayInput
      (Server : in out Server_Type;
       body : in array;
       Context : in out Swagger.Servers.Context_Type);

   --  Creates list of users with given input array
   overriding
   procedure createUsersWithListInput
      (Server : in out Server_Type;
       body : in array;
       Context : in out Swagger.Servers.Context_Type);

   --  Logs user into the system
   overriding
   procedure loginUser
      (Server : in out Server_Type;
       username : in string;
       password : in string;
       Result  : out string;
       Context : in out Swagger.Servers.Context_Type);

   --  Logs out current logged in user session
   overriding
   procedure logoutUser
      (Server : in out Server_Type
       ;
       Context : in out Swagger.Servers.Context_Type);

   --  Get user by user name
   overriding
   procedure getUserByName
      (Server : in out Server_Type;
       username : in string;
       Result  : out User;
       Context : in out Swagger.Servers.Context_Type);

   --  Updated user
   overriding
   procedure updateUser
      (Server : in out Server_Type;
       username : in string;
       body : in object;
       Context : in out Swagger.Servers.Context_Type);

   --  Delete user
   overriding
   procedure deleteUser
      (Server : in out Server_Type;
       username : in string;
       Context : in out Swagger.Servers.Context_Type);

   package Server_Impl is
      new IO.OpenAPI.Api.Skeletons.Shared_Instance (Server_Type);

end IO.OpenAPI.Api.Servers;
