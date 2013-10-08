-module(ipdevice_route_req).

-export([handle_req/2]).

-include("ipdevice.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    wh_util:put_callid(JObj),
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' -> maybe_relay_req(JObj);
        _Else -> 'ok'
    end.

-spec maybe_relay_req(wh_json:object()) -> 'ok'.
maybe_relay_req(JObj) ->
    case ipdevice_util:get_device_ccv(JObj) of
        'undefined' -> 'ok';
        Props ->
            wapi_route:publish_req(update_ccvs(Props, JObj))
    end.

-spec update_ccvs(wh_proplist(), wh_json:object()) -> wh_json:object().
update_ccvs(Props, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    J = wh_json:set_values(Props, CCVs),
    wh_json:set_value(<<"Custom-Channel-Vars">>, J, JObj).
