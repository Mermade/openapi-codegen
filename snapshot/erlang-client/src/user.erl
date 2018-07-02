-module(user).

-export([encode/1]).

-export_type([user/0]).

-type user() ::
    #{ 'id' => integer,
'username' => string,
'firstName' => string,
'lastName' => string,
'email' => string,
'password' => string,
'phone' => string,
'userStatus' => integer     }.

encode(#{ 'id' := Id,
'username' := Username,
'firstName' := FirstName,
'lastName' := LastName,
'email' := Email,
'password' := Password,
'phone' := Phone,
'userStatus' := UserStatus        }) ->
    #{ 'id' => Id,
'username' => Username,
'firstname' => FirstName,
'lastname' => LastName,
'email' => Email,
'password' => Password,
'phone' => Phone,
'userstatus' => UserStatus     }.
