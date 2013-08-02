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
         ,post/2, post/3, post/4
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(STATUS_PATH_TOKEN, <<"status">>).
-define(PINS_PATH_TOKEN, <<"pins">>).

-define(MUTE_PATH_TOKEN, <<"mute">>).
-define(UNMUTE_PATH_TOKEN, <<"unmute">>).
-define(DEAF_PATH_TOKEN, <<"deaf">>).
-define(UNDEAF_PATH_TOKEN, <<"undeaf">>).
-define(KICK_PATH_TOKEN, <<"kick">>).

-define(LOCK_CONF, <<"lock">>).
-define(UNLOCK_CONF, <<"unlock">>).
-define(MUTE_CONF, <<"mute">>).
-define(UNMUTE_CONF, <<"unmute">>).
-define(START_RECORD_CONF, <<"start_record">>).
-define(STOP_RECORD_CONF, <<"stop_record">>).
-define(HANGUP_CONF, <<"hangup">>).
-define(ADD_PARTICIPANT, <<"add_participant">>).

-define(CONF_PIN_LIST, <<"conferences/listing_by_pin">>).
-define(CONF_PIN_DOC, <<"conferences_pins">>).
-define(CONF_PIN_NUMBER, 500).
-define(CONF_PIN_NUMBER_MAX, 500).


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
    [?HTTP_GET];
allowed_methods(_, ?LOCK_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?UNLOCK_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?MUTE_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?UNMUTE_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?START_RECORD_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?STOP_RECORD_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?HANGUP_CONF) ->
    [?HTTP_POST];
allowed_methods(_, ?ADD_PARTICIPANT) ->
    [?HTTP_POST].

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
resource_exists(_, ?STATUS_PATH_TOKEN) -> 'true';
resource_exists(_, ?LOCK_CONF) -> 'true';
resource_exists(_, ?UNLOCK_CONF) -> 'true';
resource_exists(_, ?MUTE_CONF) -> 'true';
resource_exists(_, ?UNMUTE_CONF) -> 'true';
resource_exists(_, ?START_RECORD_CONF) -> 'true';
resource_exists(_, ?STOP_RECORD_CONF) -> 'true';
resource_exists(_, ?HANGUP_CONF) -> 'true';
resource_exists(_, ?ADD_PARTICIPANT) -> 'true'.

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


validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?PINS_PATH_TOKEN) ->
    load_pins(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    load_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    load_conference(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?STATUS_PATH_TOKEN) ->
    load_conference_status(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, _) ->
    load_conference(Id, Context).


validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?MUTE_PATH_TOKEN = Action, CallId) ->
    validate_command(Context, Id, Action, CallId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?DEAF_PATH_TOKEN = Action, CallId) ->
    validate_command(Context, Id, Action, CallId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?UNMUTE_PATH_TOKEN = Action, CallId) ->
    validate_command(Context, Id, Action, CallId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?UNDEAF_PATH_TOKEN = Action, CallId) ->
    validate_command(Context, Id, Action, CallId);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?KICK_PATH_TOKEN = Action, CallId) ->
    validate_command(Context, Id, Action, CallId).

-spec post(cb_context:context(), path_token()) ->
                  cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) ->
                  cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).


post(#cb_context{doc=Doc, req_data=Data, db_name=AccDb}=Context, _, ?ADD_PARTICIPANT) ->
    Participants = wh_json:get_value(<<"participants">>, Doc),
    [Pin] = get_pins(AccDb, 1),
    Participant =  wh_json:set_value(<<"pin">>, Pin, Data),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"participants">>, [Participant|Participants], Doc)}),
    Context#cb_context{resp_data=Participant};
post(#cb_context{doc=Doc}=Context, Id, ?LOCK_CONF) ->
    publish_conference_event(Id, ?LOCK_CONF),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(?LOCK_CONF, 'true', Doc)});
post(#cb_context{doc=Doc}=Context, Id, ?UNLOCK_CONF) ->
    publish_conference_event(Id, ?UNLOCK_CONF),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(?LOCK_CONF, 'false', Doc)});
post(#cb_context{doc=Doc}=Context, Id, ?MUTE_CONF) ->
    case exec_command(Id, <<"mute_participant">>) of
        {'ok', _} ->
            crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(?MUTE_CONF, 'true', Doc)});
        {'error', Error} ->
            lager:error("error while sending command ~p, ~p", [<<"mute_participant">>, Error]),
            crossbar_util:response('error', <<"command_failed">>, 500, Error, Context)
    end;
post(#cb_context{doc=Doc}=Context, Id, ?UNMUTE_CONF) ->
    case exec_command(Id, <<"unmute_participant">>) of
        {'ok', _} ->
            crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(?MUTE_CONF, 'false', Doc)});
        {'error', Error} ->
            lager:error("error while sending command ~p, ~p", [<<"unmute_participant">>, Error]),
            crossbar_util:response('error', <<"command_failed">>, 500,  Error, Context)
    end;
post(#cb_context{doc=Doc}=Context, Id, ?START_RECORD_CONF) ->
    publish_conference_event(Id, ?START_RECORD_CONF),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"record">>, 'true', Doc)});
post(#cb_context{doc=Doc}=Context, Id, ?STOP_RECORD_CONF) ->
    publish_conference_event(Id, ?STOP_RECORD_CONF),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"record">>, 'false', Doc)});
post(Context, _Id, _) ->
    Context.

post(Context, _Id, _Action, _CallId) ->
    exec_command(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec validate_command(cb_context:context(), path_token(), path_token(), path_token()) ->
                              cb_context:context().
validate_command(Context, Id, Action, CallId) ->
    Context1 = crossbar_doc:load(Id, Context),
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
            PublishFun = fun(P) -> wapi_conference:publish_command(ConfId, P) end,
            case whapps_util:amqp_pool_request(Req
                                               ,PublishFun
                                               ,fun wapi_conference:command_resp_v/1
                                              )
            of
                {'ok', RespJObj} ->
                    crossbar_util:response(wh_json:get_value(<<"Response-Message">>, RespJObj, <<"command executed successfully">>), Context);
                {'error', ErrJObj} ->
                    crossbar_util:response('error', wh_json:get_value(<<"Error-Message">>, ErrJObj), Context)
            end
    end.

-spec exec_command(ne_binary(), ne_binary()) -> {'ok', any()} | {'error', any()}.
-spec exec_command(ne_binary(), ne_binary(), ne_binary()) -> {'ok', any()} | {'error', any()}.
exec_command(Id, Action) ->
    exec_command(Id, Action, <<"non_moderator">>).
exec_command(Id, Action, Participant) ->
    Req = [{<<"Conference-ID">>, Id}
           ,{<<"Application-Name">>, Action}
           ,{<<"Participant">>, Participant}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    PublishFun = fun(P) -> wapi_conference:publish_command(Id, P) end,
    whapps_util:amqp_pool_request(Req
                                  ,PublishFun
                                  ,fun wapi_conference:command_resp_v/1
                                 ).


-spec publish_conference_event(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
publish_conference_event(ConferenceName, Action) ->
    spawn(fun() ->
            Event = [{<<"Event">>, Action}
                     ,{<<"Focus">>, wh_util:to_binary(erlang:node())}
                     ,{<<"Conference-ID">>, ConferenceName}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            Publisher = fun(P) -> wapi_conference:publish_conference_event(ConferenceName, P) end,
            whapps_util:amqp_pool_send(Event ,Publisher)
          end).



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
    Context1 = crossbar_doc:load(ConfId, Context),
    case cb_context:resp_status(Context1) of
        'success' -> lookup_status(ConfId, Context1);
        _ -> Context1
    end.

load_pins(#cb_context{db_name=AccDb}=Context) ->
    case get_pins(AccDb, 10) of
        [] ->
            lager:error("error getting pins", []),
            crossbar_util:response('error', <<"pins could not be generated">>, 500, [], Context);
        Pins ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                     ,Pins
                                    )
    end.

lookup_status(ConfId, Context) ->
    case whapps_util:amqp_pool_request([{<<"Conference-ID">>, ConfId}
                                        ,{<<"List-Participants">>, cb_context:req_value(Context, <<"list_participants">>, 'true')}
                                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                       ]
                                       ,fun wapi_conference:publish_status_req/1
                                       ,fun wapi_conference:status_resp_v/1
                                      )
    of
        {'ok', Status} ->
            CleanStatus = wh_api:remove_defaults(Status),
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                     ,clean_status(CleanStatus)
                                    );
        {'error', _E} ->
            lager:debug("error getting participants: ~p", [_E]),
            crossbar_util:response('error', <<"No conference data found">>, 404, [], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_status(wh_json:object()) -> wh_json:object().
clean_status(JObj) ->
    RemoveKeys = [<<"UUID">>],
    NJObj = clean_jobj(JObj, RemoveKeys, []),
    Participants = lists:foldl(fun(Participant, Acc) ->
                                   [clean_participant(Participant)|Acc]
                               end
                               ,[]
                               ,wh_json:get_value(<<"participants">>, NJObj, [])
                              ),
    wh_json:set_value(<<"participants">>, Participants, NJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_participant(wh_json:object()) -> wh_json:object().
clean_participant(JObj) ->
    RemoveKeys = [<<"Custom-Channel-Vars">>
                  ,<<"Switch-Hostname">>
                  ,<<"Mute-Detect">>
                 ],
    CleanKeys = [{<<"Speak">>, <<"mute">>, fun(X) -> not X end}],
    clean_jobj(JObj, RemoveKeys, CleanKeys).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_jobj(wh_json:object(), [any(), ...], [any(), ...]) -> wh_json:object().
clean_jobj(JObj, RemoveKeys, []) ->
    JObj1 = wh_json:delete_keys(RemoveKeys, JObj),
    wh_json:foldl(
        fun(K, V, Acc) ->
            wh_json:set_value(cleanup_binary(K), V, Acc)
        end
        ,wh_json:new()
        ,JObj1
    );
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey} | T]) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey, Fun} | T]) ->
    case wh_json:get_value(OldKey, JObj) of
        'undefined' ->
            J1 = wh_json:set_value(NewKey, <<"undefined">>, JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
        Value ->
            J1 = wh_json:set_value(NewKey, Fun(Value), JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_binary(ne_binary()) -> ne_binary().
cleanup_binary(Binary) ->
    String = binary:bin_to_list(Binary),
    Binary1 = binary:list_to_bin(string:to_lower(String)),
    binary:replace(Binary1, <<"-">>, <<"_">>, [global]).


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
on_successful_validation('undefined', Context) ->
    Context1 = check_pins(Context),
    Context1#cb_context{doc=wh_json:set_value(<<"pvt_type">>
                                              ,<<"conference">>
                                              ,cb_context:doc(Context1))};
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

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec check_pins(cb_context:context()) -> cb_context:context().
check_pins(Context) ->
    PvtFuns = [fun check_participants_pin/1
               ,fun check_conf_pins/1
              ],
    lists:foldl(fun(F, C) -> F(C) end, Context, PvtFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec check_participants_pin(cb_context:context()) -> cb_context:context().
check_participants_pin(#cb_context{doc=JObj, db_name=AccDb}=Context) ->
    Ps = lists:foldl(fun(Participant, Acc) ->
                        case wh_json:get_value(<<"pin">>, Participant) of
                            'undefined' ->
                                [NPin] = get_pins(AccDb, 1),
                                [wh_json:set_value(<<"pin">>, NPin, Participant)|Acc];
                            Pin ->
                                [NPin] = pin_is_unique(AccDb, [Pin]),
                                [wh_json:set_value(<<"pin">>, NPin, Participant)|Acc]
                        end
                     end
                     ,[]
                     ,wh_json:get_value(<<"participants">>, JObj, [])
                    ),
    Context#cb_context{doc=wh_json:set_value(<<"participants">>, Ps, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec check_conf_pins(cb_context:context()) -> cb_context:context().
check_conf_pins(Context) ->
    Context.


%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec get_pins(ne_binary(), integer()) -> [ne_binary(), ...] | [].
get_pins(AcctDb, N) ->
    Number = max_pin(N),
    case couch_mgr:open_doc(AcctDb, ?CONF_PIN_DOC) of
        {'ok', JObj} ->
            Pins = wh_json:get_value(<<"pins">>, JObj),
            maybe_generate_pins(AcctDb, Pins, Number);
        {'error', 'not_found'} ->
            lager:info("missing doc ~p in ~p creating...", [?CONF_PIN_DOC, AcctDb]),
            case create_conferences_pins_doc(AcctDb) of
                'ok' -> get_pins(AcctDb, Number);
                'error' -> []
            end;
        {'error', _E} ->
            lager:error("failed to open ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec maybe_generate_pins(ne_binary(), [ne_binary(), ...], integer()) -> [ne_binary(), ...].
maybe_generate_pins(AcctDb, Pins, Number) ->
    try lists:split(Number, Pins) of
        {L1, L2} ->
            NPins = generate_pins(AcctDb, Number, L2),
            case update_conferences_pins_doc(AcctDb, NPins) of
                'ok' -> L1;
                'error' -> get_pins(AcctDb, Number)
            end
    catch
        _E:'badarg' ->
            lager:error("failed to select ~p pins in ~p, creating new pins...", [Number, AcctDb]),
            NPins = generate_pins(AcctDb, (Number-erlang:length(Pins)), Pins),
            update_conferences_pins_doc(AcctDb, NPins),
            get_pins(AcctDb, Number);
        _E:_R ->
            lager:error("failed to select ~p pins in ~p, reason: ~p", [Number, AcctDb, _R]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec create_conferences_pins_doc(ne_binary()) -> 'ok' | 'error'.
create_conferences_pins_doc(AcctDb) ->
    Doc  = wh_json:set_values([{<<"_id">>, ?CONF_PIN_DOC}
                               ,{<<"pins">>, generate_pins(AcctDb, ?CONF_PIN_NUMBER)}
                              ]
                              ,wh_json:new()),
    case couch_mgr:save_doc(AcctDb, Doc) of
        {'ok', _} ->
            lager:info("~p created in ~p", [?CONF_PIN_DOC, AcctDb]),
            'ok';
        {'error', _E} ->
            lager:error("failed to create ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec update_conferences_pins_doc(ne_binary(), [ne_binary(), ...]) -> 'ok' | 'error'.
update_conferences_pins_doc(AcctDb, Pins) ->
    UpdateProps = [{<<"pins">>, Pins}],
    case couch_mgr:update_doc(AcctDb, ?CONF_PIN_DOC, UpdateProps) of
        {'ok', _} ->
            lager:debug("~p updated in ~p", [?CONF_PIN_DOC, AcctDb]),
            'ok';
        {'error', _E} ->
            lager:error("failed to update ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec generate_pins(ne_binary(), integer()) -> [ne_binary(), ...].
-spec generate_pins(ne_binary(), integer(), [ne_binary(), ...]) -> [ne_binary(), ...].
generate_pins(AcctDb, Number) ->
    generate_pins(AcctDb, Number, []).


generate_pins(AcctDb, 0, Acc) ->
    pin_is_unique(AcctDb, Acc);
generate_pins(AcctDb, Number, Acc) ->
    Pin = create_pin(),
    case lists:member(Pin, Acc) of
        'false' ->
            generate_pins(AcctDb, Number-1, [Pin|Acc]);
        'true' ->
            generate_pins(AcctDb, Number, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec max_pin(integer()) -> integer().
max_pin(Number) ->
    case Number > ?CONF_PIN_NUMBER_MAX of
        'true' ->
            lager:error("max pin limit reached, request: ~p (max: ~p)", [Number, ?CONF_PIN_NUMBER_MAX]),
            ?CONF_PIN_NUMBER_MAX;
        'false' -> Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec pin_is_unique(ne_binary(), integer()) -> [ne_binary(), ...].
pin_is_unique(AcctDb, Pins) ->
    ViewPins = get_pin_from_view(AcctDb),
    {Miss, NPins} = lists:foldl(fun(Pin, {Missing, Acc}) ->
                    case lists:member(Pin, ViewPins) of
                        'false' ->
                            {Missing, [Pin|Acc]};
                        'true' ->
                            {Missing+1, Acc}
                    end
                end
                ,{0, []}
                ,Pins
                ),
    case Miss of
        0 -> NPins;
        N ->
            lists:merge(generate_pins(AcctDb, N), NPins)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec get_pin_from_view(ne_binary()) -> [ne_binary(), ...] | 'false'.
get_pin_from_view(AcctDb) ->
    case couch_mgr:get_all_results(AcctDb, ?CONF_PIN_LIST) of
        {'ok', JObjs} ->
            couch_mgr:get_result_keys(JObjs);
        {'error', _E} ->
            lager:error("failed to check view ~p in ~p, reason: ~p", [?CONF_PIN_LIST, AcctDb, _E]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% @end
%%--------------------------------------------------------------------
-spec create_pin() -> ne_binary().
create_pin() ->
    Pin =  wh_util:to_binary(random:uniform(999999)),
    Length = erlang:length(erlang:binary_to_list(Pin)),
    case  Length < 6 of
        'true' ->
            Prefix = binary:copy(<<"0">>, 6-Length),
            <<Prefix/binary, Pin/binary>>;
        'false' -> Pin
    end.




