-module(category).

-export([encode/1]).

-export_type([category/0]).

-type category() ::
    #{ 'id' => integer,
'name' => string     }.

encode(#{ 'id' := Id,
'name' := Name        }) ->
    #{ 'id' => Id,
'name' => Name     }.
