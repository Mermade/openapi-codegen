-module(storeApi_api).

-export([getInventory/, getInventory/,
         placeOrder/, placeOrder/,
         getOrderById/, getOrderById/,
         deleteOrder/, deleteOrder/]).

-define(BASE_URL, "/v2").

%% @doc Returns pet inventories by status
%% Returns a map of status codes to quantities
-spec getInventory(ctx:ctx()) -> {ok, object, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getInventory(Ctx) ->
    getInventory(Ctx, #{}).

-spec getInventory(ctx:ctx(), maps:map()) -> {ok, object, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getInventory(Ctx, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/store/inventory"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Place an order for a pet
-spec placeOrder(ctx:ctx(), object) -> {ok, Order, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
placeOrder(Ctx, body) ->
    placeOrder(Ctx, body, #{}).

-spec placeOrder(ctx:ctx(), object, maps:map()) -> {ok, Order, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
placeOrder(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/store/order"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find purchase order by ID
%% For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions
-spec getOrderById(ctx:ctx(), integer) -> {ok, Order, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getOrderById(Ctx, orderId) ->
    getOrderById(Ctx, orderId, #{}).

-spec getOrderById(ctx:ctx(), integer, maps:map()) -> {ok, Order, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getOrderById(Ctx, orderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/store/order/{orderId}"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Delete purchase order by ID
%% For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
-spec deleteOrder(ctx:ctx(), integer) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deleteOrder(Ctx, orderId) ->
    deleteOrder(Ctx, orderId, #{}).

-spec deleteOrder(ctx:ctx(), integer, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deleteOrder(Ctx, orderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = DELETE,
    Path = ["/store/order/{orderId}"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


