%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account pin auth module
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it authorizes an account level cred
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_pin_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("../crossbar.hrl").

-define(PIN_VIEW, <<"conferences/listing_by_pin">>).

%%%===================================================================
%%% pin
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.pin_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.pin_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.pin_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.pin_auth">>, ?MODULE, 'put').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(#cb_context{req_nouns=[{<<"pin_auth">>, _}]}) -> 'true';
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"pin_auth">>, []}]}) -> 'true';
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    cb_context:validate_request_data(<<"pin_auth">>, Context, fun maybe_validate/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
put(Context) ->
    _ = spawn(fun() -> maybe_update_conference(Context) end),
    create_token(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec maybe_validate(cb_context:context()) -> cb_context:context().
maybe_validate(#cb_context{doc=JObj}=Context) ->
    AccountId = whapps_config:get(<<"crossbar.pin_auth">>, <<"account_id">>, <<>>),
    Pin = wh_json:get_value(<<"pin">>, JObj),
    AccDb = wh_util:format_account_id(AccountId, 'encoded'),
    case wh_json:is_empty(Pin)
        orelse couch_mgr:get_all_results(AccDb, ?PIN_VIEW) of
        'true' -> cb_context:add_system_error('invalid_credentials', Context);
        {'ok', JObjs} ->
                ConfId = lists:foldl(fun(J, Acc) ->
                            case wh_json:get_value(<<"key">>, J) =:= Pin of
                                'true' ->
                                    wh_json:get_value(<<"id">>, J);
                                'false' ->
                                    Acc
                            end
                         end
                         ,'undefined'
                         ,JObjs
                        ),
                case ConfId of
                    'undefined' ->
                        cb_context:add_system_error('invalid_credentials', Context);
                    Id ->
                        on_successful_validation(Context#cb_context{account_id=AccountId, db_name=AccDb}, Id)
                end;
        {'error', _E} ->
            lager:error("failed to check view ~p in ~p, reason: ~p", [?PIN_VIEW, AccDb, _E]),
            cb_context:add_system_error('datastore_unreachable', Context)
    end.


-spec on_successful_validation(cb_context:context(), ne_binary() ) -> cb_context:context().
on_successful_validation(#cb_context{doc=JObj, account_id=AccountId}=Context, ConfId) ->
    Context#cb_context{resp_status='success'
                       ,doc=wh_json:set_values([{<<"account_id">>, AccountId}
                                                ,{<<"conference_id">>, ConfId}
                                                ,{<<"is_moderator">>, is_moderator(Context, ConfId)}
                                               ], JObj)}.


is_moderator(#cb_context{doc=JObj, db_name=AccountDb}, ConfId) ->
    Pin = wh_json:get_value(<<"pin">>, JObj),
    case couch_mgr:open_doc(AccountDb, ConfId) of
        {'ok', Conf} ->
            ModPin = wh_json:get_value([<<"pins">>, <<"moderator">>], Conf),
            case  ModPin =:= Pin of
                'true' -> 'true';
                'false' ->
                    lists:foldl(fun(Participant, Acc) ->
                                    case wh_json:get_value(<<"pin">>, Participant) =:= Pin of
                                        'true' ->
                                            wh_json:get_value(<<"moderator">>, Participant, 'false');
                                        'false' -> Acc
                                    end
                                end
                                ,'false'
                                ,wh_json:get_value(<<"participants">>, Conf)
                               )
            end;
        {'error', _E} ->
            lager:error("failed to open conference ~p in ~p, reason: ~p", [ConfId, AccountDb, _E]),
            'false'
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token(cb_context:context()) -> cb_context:context().
create_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        'true' ->
            lager:debug("refusing to create auth token for an empty doc"),
            cb_context:add_system_error('invalid_credentials', Context);
        'false' ->
            AccountId = wh_json:get_value(<<"account_id">>, JObj),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {'ok', Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(crossbar_util:response_auth(JObj)
                                           ,Context#cb_context{auth_token=AuthToken
                                                               ,auth_doc=Doc
                                                              });
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error('datastore_fault', Context)
            end
    end.

maybe_update_conference(#cb_context{doc=Doc, resp_status='success', db_name=AccDb}=Context) ->
    ConfId = wh_json:get_value(<<"conference_id">>, Doc),
    case wh_json:get_value(<<"update">>, Doc) of
        'undefined' -> 'ok';
        _Update ->
            case couch_mgr:open_doc(AccDb, ConfId) of
                {'ok', JObj} ->
                    update_conference(Context, JObj);
                {'error', _R} ->
                    lager:error("failed to load conference doc ~p in ~p, reason: ~p", [ConfId, AccDb, _R])
            end
    end.


update_conference(#cb_context{doc=Doc}=Context, Conference) ->
    Pin = wh_json:get_value(<<"pin">>, Doc),
    Update = wh_json:get_value(<<"update">>, Doc),
    Participants = lists:foldl(
                        fun(P, {Status, Acc}) ->
                            case wh_json:get_value(<<"pin">>, P) =:= Pin of
                                'true' ->
                                    {'true', [wh_json:merge_jobjs(Update, P)|Acc]};
                                'false' ->
                                    {Status, [P|Acc]}
                            end
                        end
                        ,{'false', []}
                        ,wh_json:get_value(<<"participants">>, Conference, [])
                   ),
    case Participants of
        {'true', NParticipants} ->
            NConference = [{<<"participants">>, NParticipants}],
            save_conference(Context, NConference, 0);
        {'false', _} ->
            'ok'
    end.


save_conference(#cb_context{db_name=AccDb, doc=Doc}, _, 3) ->
    ConfId = wh_json:get_value(<<"conference_id">>, Doc),
    lager:error("failed to update conference ~p in ~p", [ConfId, AccDb]);
save_conference(#cb_context{db_name=AccDb, doc=Doc}=Context, Conference, Attempt) ->
    ConfId = wh_json:get_value(<<"conference_id">>, Doc),
    case couch_mgr:update_doc(AccDb, ConfId, Conference) of
        {'ok', _} ->
            'ok';
        {'error', _R} ->
            lager:error("failed to update conference doc ~p in ~p, reason: ~p attempt ~p", [ConfId, AccDb, _R, Attempt]),
            save_conference(Context, Conference, Attempt+1)
    end.












