-module(order).

-export([encode/1]).

-export_type([order/0]).

-type order() ::
    #{ 'id' => integer,
'petId' => integer,
'quantity' => integer,
'shipDate' => string,
'status' => string,
'complete' => boolean     }.

encode(#{ 'id' := Id,
'petId' := PetId,
'quantity' := Quantity,
'shipDate' := ShipDate,
'status' := Status,
'complete' := Complete        }) ->
    #{ 'id' => Id,
'petid' => PetId,
'quantity' => Quantity,
'shipdate' => ShipDate,
'status' => Status,
'complete' => Complete     }.
