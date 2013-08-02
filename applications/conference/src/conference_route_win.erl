%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(conference_route_win).

-export([handle_req/2]).

-include("conference.hrl").

handle_req(JObj, _Props) ->
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    case whapps_call:retrieve(CallId) of
        {'ok', C} ->
            Call = whapps_call:from_route_win(JObj, C),
            handle_call(Call),
            lager:info("conference wins the routing", []),
            'ok';
         {'error', _R} ->
            lager:error("something went wrong: ~p", [_R]),
            'ok'
    end.

handle_call(Call) ->
    Command = [{<<"Call">>, whapps_call:to_json(Call)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_discovery_req(Command).
