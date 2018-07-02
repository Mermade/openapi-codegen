-module(pet).

-export([encode/1]).

-export_type([pet/0]).

-type pet() ::
    #{ 'id' => integer,
'category' => object,
'name' := string,
'photoUrls' := array,
'tags' => array,
'status' => string     }.

encode(#{ 'id' := Id,
'category' := Category,
'name' := Name,
'photoUrls' := PhotoUrls,
'tags' := Tags,
'status' := Status        }) ->
    #{ 'id' => Id,
'category' => Category,
'name' => Name,
'photourls' => PhotoUrls,
'tags' => Tags,
'status' => Status     }.
