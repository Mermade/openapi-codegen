-module(userApi_api).

-export([createUser/, createUser/,
         createUsersWithArrayInput/, createUsersWithArrayInput/,
         createUsersWithListInput/, createUsersWithListInput/,
         loginUser/, loginUser/,
         logoutUser/, logoutUser/,
         getUserByName/, getUserByName/,
         updateUser/, updateUser/,
         deleteUser/, deleteUser/]).

-define(BASE_URL, "/v2").

%% @doc Create user
%% This can only be done by the logged in user.
-spec createUser(ctx:ctx(), object) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUser(Ctx, body) ->
    createUser(Ctx, body, #{}).

-spec createUser(ctx:ctx(), object, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUser(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/user"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Creates list of users with given input array
-spec createUsersWithArrayInput(ctx:ctx(), array) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUsersWithArrayInput(Ctx, body) ->
    createUsersWithArrayInput(Ctx, body, #{}).

-spec createUsersWithArrayInput(ctx:ctx(), array, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUsersWithArrayInput(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/user/createWithArray"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Creates list of users with given input array
-spec createUsersWithListInput(ctx:ctx(), array) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUsersWithListInput(Ctx, body) ->
    createUsersWithListInput(Ctx, body, #{}).

-spec createUsersWithListInput(ctx:ctx(), array, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
createUsersWithListInput(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/user/createWithList"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Logs user into the system
-spec loginUser(ctx:ctx(), string, string) -> {ok, string, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
loginUser(Ctx, username, password) ->
    loginUser(Ctx, username, password, #{}).

-spec loginUser(ctx:ctx(), string, string, maps:map()) -> {ok, string, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
loginUser(Ctx, username, password, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/user/login"],
    QS = lists:flatten([, ])++IO.OpenAPI_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Logs out current logged in user session
-spec logoutUser(ctx:ctx()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
logoutUser(Ctx) ->
    logoutUser(Ctx, #{}).

-spec logoutUser(ctx:ctx(), maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
logoutUser(Ctx, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/user/logout"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get user by user name
-spec getUserByName(ctx:ctx(), string) -> {ok, User, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getUserByName(Ctx, username) ->
    getUserByName(Ctx, username, #{}).

-spec getUserByName(ctx:ctx(), string, maps:map()) -> {ok, User, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getUserByName(Ctx, username, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/user/{username}"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Updated user
%% This can only be done by the logged in user.
-spec updateUser(ctx:ctx(), string, object) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updateUser(Ctx, username, body) ->
    updateUser(Ctx, username, body, #{}).

-spec updateUser(ctx:ctx(), string, object, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updateUser(Ctx, username, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = PUT,
    Path = ["/user/{username}"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Delete user
%% This can only be done by the logged in user.
-spec deleteUser(ctx:ctx(), string) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deleteUser(Ctx, username) ->
    deleteUser(Ctx, username, #{}).

-spec deleteUser(ctx:ctx(), string, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deleteUser(Ctx, username, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = DELETE,
    Path = ["/user/{username}"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


