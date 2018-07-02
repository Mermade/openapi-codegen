-module(tag).

-export([encode/1]).

-export_type([tag/0]).

-type tag() ::
    #{ 'id' => integer,
'name' => string     }.

encode(#{ 'id' := Id,
'name' := Name        }) ->
    #{ 'id' => Id,
'name' => Name     }.
