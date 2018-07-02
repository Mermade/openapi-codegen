-module(api_response).

-export([encode/1]).

-export_type([api_response/0]).

-type api_response() ::
    #{ 'code' => integer,
'type' => string,
'message' => string     }.

encode(#{ 'code' := Code,
'type' := Type,
'message' := Message        }) ->
    #{ 'code' => Code,
'type' => Type,
'message' => Message     }.
