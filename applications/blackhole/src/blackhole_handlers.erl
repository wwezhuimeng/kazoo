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
    Event = wh_json:get_value(<<"Event-Name">>, JObj),
    fw_conf_event(Event, JObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================
fw_conf_event(<<"participant_event">>, JObj) ->
    CleanJObj = clean_participant_event(JObj),
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    Id = wh_json:get_value(<<"Call-ID">>, JObj),
    blackhole_ws:broadcast_event(wh_json:get_value(<<"Conference-ID">>, JObj)
                                 ,Event
                                 ,[Id, CleanJObj]
                                );
fw_conf_event(<<"conference_event">>, JObj) ->
    ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    blackhole_ws:broadcast_event(ConfId
                                 ,Event
                                 ,[ConfId]
                                );
fw_conf_event(Event, _JObj) ->
    lager:debug("receive unknown event: ~p~n", [Event]).

clean_participant_event(JObj) ->
	RemoveKeys = [<<"Focus">>
                  ,<<"App-Version">>
                  ,<<"App-Name">>
                  ,<<"Event-Category">>
                  ,<<"Event-Name">>
                  ,<<"Msg-ID">>
                  ,<<"Node">>
                  ,<<"Server-ID">>
                  ,<<"Switch-Hostname">>
                 ],
    CleanKeys = [{<<"Mute-Detect">>, <<"mute_detect">>, fun wh_util:to_boolean/1}
                 ,{<<"Energy-Level">>, <<"energy_level">>, fun wh_util:to_integer/1}
                 ,{<<"Current-Energy">>, <<"current_energy">>, fun wh_util:to_integer/1}
                 ,{<<"Talking">>, <<"talking">>, fun wh_util:to_boolean/1}
                 ,{<<"Speak">>, <<"speak">>, fun wh_util:to_boolean/1}
                 ,{<<"Hear">>, <<"hear">>, fun wh_util:to_boolean/1}
                 ,{<<"Video">>, <<"video">>, fun wh_util:to_boolean/1}
                 ,{<<"Floor">>, <<"floor">>, fun wh_util:to_boolean/1}
                 ,{<<"Event">>, <<"event">>, fun cleanup_binary/1}
                ],
    clean_jobj(JObj, RemoveKeys, CleanKeys).


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


cleanup_binary(Binary) ->
	String = binary:bin_to_list(Binary),
	Binary1 = binary:list_to_bin(string:to_lower(String)),
	binary:replace(Binary1, <<"-">>, <<"_">>, [global]).









