-module(ipdevice_util).

-export([get_device_ccv/1,lookup_device/3]).

-include("ipdevice.hrl").

-spec get_device_ccv(wh_json:object()) -> 'undefined' | wh_proplist().
get_device_ccv(JObj) ->
    %% TODO: cache me...
    Realm = get_from_realm(JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountDb} ->
            lookup_device(AccountDb, Realm, JObj)
    end.

-spec lookup_device(api_binary(), api_binary(), wh_json:object()) -> 'undefined' | wh_proplist().
lookup_device('undefined', _, _) -> 'undefined';
lookup_device(_, 'undefined', _) -> 'undefined';
lookup_device(AccountDb, Realm, JObj) ->
    IP = wh_json:get_first_defined([<<"From-Network-Addr">>
                                    ,<<"Orig-IP">>
                                   ], JObj),
    ViewOptions = [{'key', IP}],
    case couch_mgr:get_results(?WH_SIP_DB, ?DEVICE_BY_IP, ViewOptions) of
        {'ok', [Doc]} ->
	    AccountId = wh_json:get_value([<<"value">>,  <<"account_id">>],
					  Doc),
	    case wh_util:format_account_id(AccountId, 'encoded') of
		AccountDb ->
		    [{<<"Inception">>, <<"on-net">>}
		     ,{<<"Authorizing-Type">>, <<"device">>}
		     ,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, Doc)}
		     ,{<<"Owner-ID">>, wh_json:get_value([<<"value">>, <<"owner_id">>], Doc)}
		     ,{<<"Realm">>, Realm}
		     ,{<<"Account-ID">>, AccountId}
		    ];
		_ ->
		    lager:format("wrong realm ~s ~s",[AccountDb, AccountId])
	    end;
	_Else -> io:format("No result ~p~n",[_Else]), 'undefined'
		     
    end.

-spec get_from_realm(api_binary() | wh_json:object()) -> 'undefined'.
get_from_realm('undefined') -> 'undefined';
get_from_realm(From) when is_binary(From) ->
    case binary:split(From, <<"@">>) of
        [_, Realm] -> Realm;
        _Else -> 'undefined'
    end;
get_from_realm(JObj) ->
    get_from_realm(wh_json:get_value(<<"From">>, JObj)).
