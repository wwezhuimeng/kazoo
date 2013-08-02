%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(conference_route_req).

-export([handle_req/2]).

-include("conference.hrl").

-define(CB_LIST, <<"conferences_servers/listing_by_number">>).

handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    Call = whapps_call:from_route_req(JObj),
    case known_number(Call) of
        'false' ->
            lager:debug("conference does not know what to do with this!", []);
        'true' ->
            lager:debug("conference knows what to do with this!", []),
            ControllerQ = props:get_value('queue', Props),
            send_route_response(ControllerQ, JObj),
            whapps_call:cache(Call)
    end.

known_number(Call) ->
    PropCall = whapps_call:to_proplist(Call),
    Number = props:get_value(<<"To-User">>, PropCall),
    DbName = props:get_value(<<"Account-DB">>, PropCall),
    case couch_mgr:get_results(DbName, ?CB_LIST) of
        {'error', _E} -> 
            lager:error("could not load view ~p in ~p, error: ", [?CB_LIST, DbName, _E]),
            'false';
        {'ok', JObjs} ->
            Numbers = couch_mgr:get_result_keys(JObjs),
            lists:member(Number, Numbers) 
    end.

-spec send_route_response(ne_binary(), wh_json:json()) -> 'ok'.
send_route_response(ControllerQ, JObj) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("conference knows how to route the call! sent park response").


