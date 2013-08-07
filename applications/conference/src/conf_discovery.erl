%%%============================================================================
%%% @copyright (C) 2011-2013 2600Hz Inc
%%% @doc
%%% This module is responsible for the second stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   James Aimonetti <james@2600hz.org>
%%%============================================================================
-module(conf_discovery).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([handle_discovery_req/2
         ,handle_config_req/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, 'handle_discovery_req'}
                      ,[{<<"conference">>, <<"discovery_req">>}]
                     }
                     ,{{?MODULE, 'handle_config_req'}
                       ,[{<<"conference">>, <<"config_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['discovery', 'config']}]}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"conference_discovery">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include("conference.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

-spec handle_discovery_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_discovery_req(JObj, _) ->
    'true' = wapi_conference:discovery_req_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    put('callid', whapps_call:call_id(Call)),
    case conf_participant_sup:start_participant(Call) of
        {'ok', Srv} ->
            conf_participant:set_discovery_event(JObj, Srv),
            conf_participant:consume_call_events(Srv),
            whapps_call_command:answer(Call),
            welcome_to_conference(Call, Srv, JObj);
        _Else ->
            discovery_failed(Call, 'undefined')
    end.

handle_config_req(JObj, _Props) ->
    'true' = wapi_conference:config_req_v(JObj),
    ConfigName = wh_json:get_value(<<"Profile">>, JObj),
    case whapps_config:get(<<"conferences">>, [<<"profiles">>, ConfigName]) of
        'undefined' -> lager:debug("no profile defined for ~s", [ConfigName]);
        Profile ->
            lager:debug("profile ~s found", [ConfigName]),
            Resp = [{<<"Profiles">>, wh_json:from_list([{ConfigName, Profile}])}
                    ,{<<"Caller-Controls">>, caller_controls(ConfigName)}
                    ,{<<"Advertise">>, advertise(ConfigName)}
                    ,{<<"Chat-Permissions">>, chat_permissions(ConfigName)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_conference:publish_config_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                                ,props:filter_undefined(Resp)
                                               )
    end.

caller_controls(ConfigName) ->
    case whapps_config:get(<<"conferences">>, [<<"caller-controls">>, ConfigName]) of
        'undefined' -> 'undefined';
        Controls -> wh_json:from_list([{ConfigName, Controls}])
    end.

advertise(ConfigName) ->
    case whapps_config:get(<<"conferences">>, [<<"advertise">>, ConfigName]) of
        'undefined' -> 'undefined';
        Advertise -> wh_json:from_list([{ConfigName, Advertise}])
    end.

chat_permissions(ConfigName) ->
    case whapps_config:get(<<"conferences">>, [<<"chat-permissions">>, ConfigName]) of
        'undefined' -> 'undefined';
        Chat -> wh_json:from_list([{ConfigName, Chat}])
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:debug("starting new conference discovery process"),
    {'ok', 'ok'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("conference discovery ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec welcome_to_conference(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
welcome_to_conference(Call, Srv, DiscoveryJObj) ->
    whapps_call_command:prompt(<<"conf-welcome">>, Call),
    collect_conference_pin(Call, Srv, DiscoveryJObj, 1).

collect_conference_pin(Call, _, _, Try) when Try > 3 ->
    lager:debug("caller has failed to provide a valid conference number to many times"),
    _ = whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
collect_conference_pin(Call, Srv, DiscoveryJObj, Try) ->
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_number">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            AccountDb = whapps_call:account_db(Call),
            ViewOptions = [{'key', Digits}
                           ,'include_docs'
                          ],
            case couch_mgr:get_results(AccountDb, <<"conferences/listing_by_pin">>, ViewOptions) of
                {'ok', [JObj]} ->
                    lager:debug("caller has entered a valid conference id, building object"),
                    prepare_whapps_conference(JObj
                                              ,whapps_call:set_custom_channel_var(<<"pin">>, Digits, Call)
                                              ,Srv);
                _Else ->
                    lager:debug("could not find conference number ~s: ~p", [Digits, _Else]),
                    _ = whapps_call_command:prompt(<<"conf-bad_conf">>, Call),
                    collect_conference_pin(Call, Srv, DiscoveryJObj, Try + 1)
            end
    end.

-spec prepare_whapps_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
prepare_whapps_conference(JObj, Call, Srv) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Conference = whapps_conference:from_conference_doc(Doc),
    Routines = [fun(C) -> whapps_conference:set_application_version(<<"2.0.0">>, C) end
                ,fun(C) -> whapps_conference:set_application_name(<<"conferences">>, C) end
                ,fun(C) -> 
                    Participant = wh_json:get_value(<<"value">>, JObj, wh_json:new()),
                    whapps_conference:set_participant(Participant, C) 
                end
               ],
    C = whapps_conference:update(Routines, Conference),
    search_for_conference(C, Call, Srv).

-spec search_for_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
search_for_conference(Conference, Call, Srv) ->
    case whapps_conference_command:search(Conference) of
        {'error', _} ->
            handle_search_error(Conference, Call, Srv);
        {'ok', JObj} ->
            handle_search_resp(JObj, Conference, Call, Srv)
    end.

-spec handle_search_error(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_error(Conference, Call, Srv) ->
    timer:sleep(crypto:rand_uniform(2000, 3000)),
    case whapps_conference_command:search(Conference) of
        {'error', _} ->
            lager:debug("creating conference on switch nodename '~p'", [whapps_call:switch_hostname(Call)]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv);
        {'ok', JObj} ->
            handle_search_resp(JObj, Conference, Call, Srv)
    end.

-spec handle_search_resp(wh_json:object(), whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_resp(JObj, Conference, Call, Srv) ->
    lager:debug("participant switch nodename ~p", [whapps_call:switch_hostname(Call)]),
    SwitchHostname = whapps_call:switch_hostname(Call),
    case wh_json:get_value(<<"Switch-Hostname">>, JObj) of
        SwitchHostname ->
            lager:debug("running conference is on the same switch, joining on ~s", [SwitchHostname]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv);
        _Else ->
            lager:debug("running conference is on a different switch, bridging to ~s: ~p", [_Else, JObj]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_remote(Srv, JObj)
    end.

-spec discovery_failed(whapps_call:call(), pid()) -> 'ok'.
discovery_failed(Call, _) -> whapps_call_command:hangup(Call).

-spec negotiate_focus(ne_binary(), whapps_conference:conference(), whapps_call:call()) ->
                             {'ok', wh_json:object()} |
                             {'error', term()}.
negotiate_focus(SwitchHostname, Conference, Call) ->
    AccountDb = whapps_call:account_db(Call),
    JObj = whapps_conference:conference_doc(Conference),
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"focus">>, SwitchHostname, JObj)).
