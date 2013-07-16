%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Conferences module
%%%
%%% Handle client requests for conference documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_conferences).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1
         ,post/2, post/4
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(STATUS_PATH_TOKEN, <<"status">>).

-define(MUTE_PATH_TOKEN, <<"mute">>).
-define(UNMUTE_PATH_TOKEN, <<"unmute">>).
-define(DEAF_PATH_TOKEN, <<"deaf">>).
-define(UNDEAF_PATH_TOKEN, <<"undeaf">>).
-define(KICK_PATH_TOKEN, <<"kick">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.conferences">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.conferences">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.conferences">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.conferences">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.conferences">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.conferences">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() ->
                             http_methods().
-spec allowed_methods(path_token()) ->
                             http_methods().
-spec allowed_methods(path_token(), path_token()) ->
                             http_methods().
-spec allowed_methods(path_token(), path_token(), path_token() | integer()) ->
                             http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_, ?STATUS_PATH_TOKEN) ->
    [?HTTP_GET].

allowed_methods(_, ?MUTE_PATH_TOKEN, _) ->
    [?HTTP_POST];
allowed_methods(_, ?DEAF_PATH_TOKEN, _) ->
    [?HTTP_POST];
allowed_methods(_, ?UNMUTE_PATH_TOKEN, _) ->
    [?HTTP_POST];
allowed_methods(_, ?UNDEAF_PATH_TOKEN, _) ->
    [?HTTP_POST];
allowed_methods(_, ?KICK_PATH_TOKEN, _) ->
    [?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) ->
                             'true'.
-spec resource_exists(path_token(), path_token()) ->
                             'true'.
-spec resource_exists(path_token(), path_token(), path_token() | integer()) ->
                             'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?STATUS_PATH_TOKEN) -> 'true'.

resource_exists(_, ?MUTE_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?DEAF_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?UNMUTE_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?UNDEAF_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?KICK_PATH_TOKEN, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_conference_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create_conference(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    load_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    load_conference(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?STATUS_PATH_TOKEN) ->
    load_conference_status(Id, Context).

validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?MUTE_PATH_TOKEN = Action, PId) ->
    validate_command(Context, Id, Action, PId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?DEAF_PATH_TOKEN = Action, PId) ->
    validate_command(Context, Id, Action, PId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?UNMUTE_PATH_TOKEN = Action, PId) ->
    validate_command(Context, Id, Action, PId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?UNDEAF_PATH_TOKEN = Action, PId) ->
    validate_command(Context, Id, Action, PId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?KICK_PATH_TOKEN = Action, PId) ->
    validate_command(Context, Id, Action, PId).

-spec post(cb_context:context(), path_token()) ->
                  cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

post(Context, _Id, _Action, _CallId) ->
    exec_command(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

-spec validate_command(cb_context:context(), path_token(), path_token(), path_token()) ->
                              cb_context:context().
validate_command(Context, Id, Action, CallId) ->
    Context1 = cb_context:load(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            Exec = {action_to_app(Action), Id, CallId},
            crossbar_util:response([Action, " caller ", CallId]
                                   ,cb_context:store('exec', Exec, Context1)
                                  );
        _ ->
            lager:debug("failed to find conference definition ~s", [Id]),
            cb_context:add_validation_error(<<"conference_id">>, <<"required">>, <<"not found">>, Context)
    end.

-spec action_to_app(path_token()) -> ne_binary().
action_to_app(?MUTE_PATH_TOKEN) ->
    <<"mute_participant">>;
action_to_app(?DEAF_PATH_TOKEN) ->
    <<"deaf_participant">>;
action_to_app(?UNMUTE_PATH_TOKEN) ->
    <<"unmute_participant">>;
action_to_app(?UNDEAF_PATH_TOKEN) ->
    <<"undeaf_participant">>;
action_to_app(?KICK_PATH_TOKEN) ->
    <<"kick">>.

-spec exec_command(cb_context:context()) -> cb_context:context().
exec_command(Context) ->
    case cb_context:fetch('exec', Context) of
        {App, ConfId, CallId} ->
            lager:debug("exec ~s on ~s in ~s", [App, CallId, ConfId]),
            Req = [{<<"Conference-ID">>, ConfId}
                   ,{<<"Application-Name">>, App}
                   ,{<<"Call-ID">>, CallId}
                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ],
            case whapps_util:amqp_pool_request(Req
                                               ,fun wapi_conference:publish_command/1
                                               ,fun wapi_conference:command_resp_v/1
                                              )
            of
                {'ok', RespJObj} ->
                    crossbar_util:response(wh_json:get_value(<<"Response-Message">>, RespJObj, <<"command executed successfully">>), Context);
                {'error', ErrJObj} ->
                    crossbar_util:response(wh_json:get_value(<<"Error-Message">>, ErrJObj, <<"command sent">>), Context)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_conference_summary(cb_context:context()) -> cb_context:context().
load_conference_summary(#cb_context{req_nouns=Nouns}=Context) ->
    case lists:nth(2, Nouns) of
        {<<"users">>, [UserId]} ->
            Filter = fun(J, A) ->
                             normalize_users_results(J, A, UserId)
                     end,
            crossbar_doc:load_view(?CB_LIST, [], Context, Filter);
        {?WH_ACCOUNTS_DB, _} ->
            crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
        _ ->
            cb_context:add_system_error('faulty_request', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a conference document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_conference(ne_binary(), cb_context:context()) -> cb_context:context().
load_conference(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

load_conference_status(ConfId, Context) ->
    case whapps_util:amqp_pool_request([{<<"Conference-ID">>, ConfId}
                                        ,{<<"List-Participants">>, cb_context:req_value(Context, <<"list_participants">>, 'true')}
                                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                       ]
                                       ,fun wapi_conference:publish_status_req/1
                                       ,fun wapi_conference:status_resp_v/1
                                      )
    of
        {'ok', Status} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                     ,wh_api:remove_defaults(Status)
                                    );
        {'error', _E} ->
            lager:debug("error getting participants: ~p", [_E]),
            crossbar_util:response('error', <<"No conference data found">>, 500, [], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_conference(ne_binary(), cb_context:context()) -> cb_context:context().
update_conference(DocId, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"conference">>, JObj)};
on_successful_validation(DocId, #cb_context{}=Context) ->
    crossbar_doc:load_merge(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_users_results(wh_json:object(), wh_json:objects(), ne_binary()) ->
                                          [api_object(),...] | [].
normalize_users_results(JObj, Acc, UserId) ->
    case wh_json:get_value([<<"value">>, <<"owner_id">>], JObj) of
        'undefined' -> normalize_view_results(JObj, Acc);
        UserId -> normalize_view_results(JObj, Acc);
        _ -> ['undefined'|Acc]
    end.
