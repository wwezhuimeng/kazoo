%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_route_req).

-export([init/0, handle_route_req/2]).

-include("reg.hrl").

init() -> whapps_maintenance:refresh(?WH_SIP_DB).

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, _Props) ->
    true = wapi_route:req_v(JObj),
    maybe_replay_route_req(JObj, wh_json:get_value(<<"From-Network-Addr">>, JObj),get_from_realm(JObj)).

-spec maybe_replay_route_req(wh_json:object(), api_binary(),api_binary()) -> any().
maybe_replay_route_req(_JObj, 'undefined', _Realm) -> 'ok';
maybe_replay_route_req(_JObj, _, 'undefined') -> 'ok';
maybe_replay_route_req(JObj, IP, Realm) ->
    lager:debug("trying to see if this route req is an auth-by-ip'd device: ~s", [IP]),

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    
    maybe_replay_route_req(JObj, IP, Realm, wh_json:get_ne_value(<<"Account-ID">>, CCVs), CCVs).

-spec maybe_replay_route_req(wh_json:object(), ne_binary(), ne_binary(), api_binary(), wh_json:object()) -> any().
maybe_replay_route_req(JObj, IP, Realm, undefined, CCVs) ->
    case lookup_ip(IP) of
        {ok, []} ->
            lager:debug("no entry in ~s for IP ~s", [?WH_SIP_DB, IP]);
        {ok, [Doc|_]} ->
            AccountID = wh_json:get_value([<<"value">>,  <<"account_id">>], Doc),
	    AccountDb = wh_util:format_account_id(AccountID, 'encoded'),
	    OwnerID = wh_json:get_value([<<"value">>, <<"owner_id">>], Doc),
	    AuthType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], Doc, <<"anonymous">>),
	    %% Check account for the caller domain and the device account are the same
	    case whapps_util:get_account_by_realm(Realm) of
		AccountID ->
		    CCVProps = [{<<"Account-ID">>, AccountID}
				,{<<"Owner-ID">>, OwnerID}
				,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, Doc)}
				,{<<"Inception">>, <<"on-net">>}
				,{<<"Authorizing-Type">>, AuthType}
			       ],
		    CCVs1 = wh_json:set_values(
			      props:filter_undefined(CCVProps),CCVs),
		    lager:debug("adding account ~s and owner ~s to ccvs", [AccountID, OwnerID]),
		    JObj1 = wh_json:set_value(<<"Custom-Channel-Vars">>, CCVs1, JObj),
		    lager:debug("replaying route_req"),
		    wapi_route:publish_req(JObj1);
		Other ->
		    lager:debug("accountid does not match ~s: ~p",[AccountDb,Other])
	    end;
        {error, _E} ->
            lager:debug("failed to lookup by ip: ~s: ~p", [IP, _E])
    end;
maybe_replay_route_req(_JObj, _IP, _Realm, _AcctId, _CCVs) ->
    ok.

-spec lookup_ip(ne_binary()) -> {'ok', list()} | {error, any()}.
lookup_ip(IP) ->
    couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, 
			   [{<<"key">>,IP}]).

-spec get_from_realm(api_binary() | wh_json:object()) -> 'undefined'.
get_from_realm('undefined') -> 'undefined';
get_from_realm(From) when is_binary(From) ->
    case binary:split(From, <<"@">>) of
        [_, Realm] -> Realm;
        _Else -> 'undefined'
    end;
get_from_realm(JObj) ->
    get_from_realm(wh_json:get_value(<<"From">>, JObj)).
