package IO.OpenAPI.Api;

import groovy.transform.Canonical
import IO.OpenAPI.Model.Default;
@Canonical
class Order {

    integer id = 

    integer petId = 

    integer quantity = 

    string shipDate = 

  /* Order Status */
  string status = 

    boolean complete = false
  

}

@Canonical
class Category {

    integer id = 

    string name = 
  

}

@Canonical
class User {

    integer id = 

    string username = 

    string firstName = 

    string lastName = 

    string email = 

    string password = 

    string phone = 

  /* User Status */
  integer userStatus = 
  

}

@Canonical
class Tag {

    integer id = 

    string name = 
  

}

@Canonical
class Pet {

    integer id = 

    object category = 

    string name = 

    array photoUrls = 

    array tags = 

  /* pet status in the store */
  string status = 
  

}

@Canonical
class ApiResponse {

    integer code = 

    string type = 

    string message = 
  

}

