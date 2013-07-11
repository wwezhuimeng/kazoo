%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_handlers).

-export([handle_conference_event/2]).

-include("blackhole.hrl").

handle_conference_event(JObj, _Props) ->
    fw_conf_event(JObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================
fw_conf_event(JObj) ->
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    io:format("CONF EVENT: ~p~n", [EventName]),
    fw_conf_event(EventName, JObj).

fw_conf_event(<<"participants_event">>, JObj) ->
    Participants = wh_json:get_value(<<"Participants">>, JObj, []),
    NParticipants = lists:foldl(
        fun(Participant, Acc) ->
            Req = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Participant)}
                   |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ],
            {'ok', Resp} = whapps_util:amqp_pool_request(Req
                                                 ,fun wapi_call:publish_call_status_req/1
                                                 ,fun wapi_call:call_status_resp_v/1
                                                ),
            MergedJObjs = clean_event(<<"participants_event">>
            						  ,wh_json:merge_jobjs(Participant, Resp)
            						 ),
            [MergedJObjs|Acc]
        end
        ,[]
        ,Participants
    ),
    blackhole_ws:broadcast_event(wh_json:get_value(<<"Conference-ID">>, JObj)
                                 ,<<"user_connected">>
                                 ,NParticipants);
fw_conf_event('undefined', _JObj) ->
    lager:error("conf event undefined", []);
fw_conf_event(_Event, _JObj) ->
    lager:debug("ignore conf event: ~p", [_Event]).











clean_event(<<"participants_event">>, JObj) ->
	RemoveKeys = [<<"App-Name">>
                  ,<<"App-Version">>
                  ,<<"Call-ID">>
                  ,<<"Event-Category">>
                  ,<<"Event-Name">>
                  ,<<"Msg-ID">>
                  ,<<"Node">>
                  ,<<"Server-ID">>
                  ,<<"Switch-Hostname">>
                 ],
    clean_jobj(JObj, RemoveKeys).

clean_jobj(JObj, RemoveKeys) ->
	clean_jobj(JObj, RemoveKeys, []).


clean_jobj(JObj, RemoveKeys, []) ->
	JObj1 = wh_json:delete_keys(RemoveKeys, JObj),
    wh_json:foldl(
    	fun(K, V, Acc) ->
    		wh_json:set_value(cleanup_binary(K), V, Acc)
    	end
    	,wh_json:new()
    	,JObj1
    );
clean_jobj(JObj, _, [{OldKey, NewKey} | T]) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean_jobj(wh_json:delete_key(OldKey, J1), T);
clean_jobj(JObj, _, [{OldKey, NewKey, Fun} | T]) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Fun(Value), JObj),
    clean_jobj( wh_json:delete_key(OldKey, J1), T).


cleanup_binary(Binary) ->
	String = binary:bin_to_list(Binary),
	Binary1 = binary:list_to_bin(string:to_lower(String)),
	binary:replace(Binary1, <<"-">>, <<"_">>, [global]).









