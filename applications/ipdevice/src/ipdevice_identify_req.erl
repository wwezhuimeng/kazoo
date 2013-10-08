-module(ipdevice_identify_req).

-export([handle_req/2]).

-include("ipdevice.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_authz:identify_req_v(JObj),
    wh_util:put_callid(JObj),
    case ipdevice_util:get_device_ccv(JObj) of
        'undefined' -> 'ok';
        Props -> publish_identify_resp(Props, JObj)
    end.

publish_identify_resp(Props, JObj) -> 
    AccountId = props:get_value(<<"Account-ID">>, Props),
    lager:debug("channel identified as account ~s", [AccountId]),
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
              ,{<<"IP-Device">>, <<"true">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_authz:publish_identify_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
