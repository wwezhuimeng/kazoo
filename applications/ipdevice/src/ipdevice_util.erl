-module(ipdevice_util).

-export([get_device_ccv/1]).

-include("ipdevice.hrl").

-spec get_device_ccv(wh_json:object()) -> 'undefined' | wh_proplist().
get_device_ccv(JObj) ->
    %% TODO: cache me...
    Realm = get_from_realm(JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountId} ->
            lookup_device(AccountId, Realm, JObj)
    end.

-spec lookup_device(api_binary(), api_binary(), wh_json:object()) -> 'undefined' | wh_proplist().
lookup_device('undefined', _, _) -> 'undefined';
lookup_device(_, 'undefined', _) -> 'undefined';
lookup_device(AccountId, Realm, JObj) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    IP = wh_json:get_first_defined([<<"From-Network-Addr">>
                                    ,<<"Orig-IP">>
                                   ], JObj),
    ViewOptions = [{'key', IP}
                   ,{'limit', 1}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?DEVICE_BY_IP, ViewOptions) of
        {'ok', [JObj]} ->
            [{<<"Inception">>, <<"on-net">>}
             ,{<<"Authorizing-Type">>, <<"device">>}
             ,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, JObj)}
             ,{<<"Owner-ID">>, wh_json:get_value([<<"doc">>, <<"owner_id">>], JObj)}
             ,{<<"Realm">>, Realm}
             ,{<<"Account-ID">>, AccountId}
            ];
         _Else -> 'undefined'
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
