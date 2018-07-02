-module(petApi_api).

-export([addPet/, addPet/,
         updatePet/, updatePet/,
         findPetsByStatus/, findPetsByStatus/,
         findPetsByTags/, findPetsByTags/,
         getPetById/, getPetById/,
         updatePetWithForm/, updatePetWithForm/,
         deletePet/, deletePet/,
         uploadFile/, uploadFile/]).

-define(BASE_URL, "/v2").

%% @doc Add a new pet to the store
-spec addPet(ctx:ctx(), object) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
addPet(Ctx, body) ->
    addPet(Ctx, body, #{}).

-spec addPet(ctx:ctx(), object, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
addPet(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Update an existing pet
-spec updatePet(ctx:ctx(), object) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updatePet(Ctx, body) ->
    updatePet(Ctx, body, #{}).

-spec updatePet(ctx:ctx(), object, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updatePet(Ctx, body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = PUT,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec findPetsByStatus(ctx:ctx(), array) -> {ok, array, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
findPetsByStatus(Ctx, status) ->
    findPetsByStatus(Ctx, status, #{}).

-spec findPetsByStatus(ctx:ctx(), array, maps:map()) -> {ok, array, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
findPetsByStatus(Ctx, status, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/pet/findByStatus"],
    QS = lists:flatten([])++IO.OpenAPI_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by tags
%% Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec findPetsByTags(ctx:ctx(), array) -> {ok, array, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
findPetsByTags(Ctx, tags) ->
    findPetsByTags(Ctx, tags, #{}).

-spec findPetsByTags(ctx:ctx(), array, maps:map()) -> {ok, array, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
findPetsByTags(Ctx, tags, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/pet/findByTags"],
    QS = lists:flatten([])++IO.OpenAPI_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find pet by ID
%% Returns a single pet
-spec getPetById(ctx:ctx(), integer) -> {ok, Pet, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getPetById(Ctx, petId) ->
    getPetById(Ctx, petId, #{}).

-spec getPetById(ctx:ctx(), integer, maps:map()) -> {ok, Pet, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
getPetById(Ctx, petId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = GET,
    Path = ["/pet/{petId}"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Updates a pet in the store with form data
-spec updatePetWithForm(ctx:ctx(), integer) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updatePetWithForm(Ctx, petId) ->
    updatePetWithForm(Ctx, petId, #{}).

-spec updatePetWithForm(ctx:ctx(), integer, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
updatePetWithForm(Ctx, petId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/pet/{petId}"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/x-www-form-urlencoded">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Deletes a pet
-spec deletePet(ctx:ctx(), integer) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deletePet(Ctx, petId) ->
    deletePet(Ctx, petId, #{}).

-spec deletePet(ctx:ctx(), integer, maps:map()) -> {ok, [], IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
deletePet(Ctx, petId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = DELETE,
    Path = ["/pet/{petId}"],
    QS = [],
    Headers = []++IO.OpenAPI_utils:optional_params(['api_key'], _OptionalParams),
    Body1 = [],
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/json">>, <<"application/x-www-form-urlencoded">>, <<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc uploads an image
-spec uploadFile(ctx:ctx(), integer) -> {ok, ApiResponse, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
uploadFile(Ctx, petId) ->
    uploadFile(Ctx, petId, #{}).

-spec uploadFile(ctx:ctx(), integer, maps:map()) -> {ok, ApiResponse, IO.OpenAPI_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), IO.OpenAPI_utils:response_info()}.
uploadFile(Ctx, petId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = POST,
    Path = ["/pet/{petId}/uploadImage"],
    QS = [],
    Headers = [],
    Body1 = body,
    ContentTypeHeader = IO.OpenAPI_utils:select_header_content_type([<<"application/octet-stream">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    IO.OpenAPI_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


