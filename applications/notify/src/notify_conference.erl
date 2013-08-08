%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz INC
%%% @doc
%%% @end
%%%
%%% @contributors
%%% Peter Defebvre
%%%
%%%-------------------------------------------------------------------
-module(notify_conference).

-export([init/0, handle_req/2]).

-include("notify.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    lager:debug("init done for ~s", [?MODULE]).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = wapi_notifications:conference_v(JObj),
    Event = wh_json:get_value(<<"Type">>, JObj),
    handle_conf_event(Event, JObj, Props).

-spec handle_conf_event(ne_binary(), wh_json:object(), proplist()) -> any().
handle_conf_event(<<"invite">>, JObj, _Props) ->
    io:format("INVITE ~p~n", [JObj]),
    'ok';
handle_conf_event(_Event, _JObj, _Props) ->
    lager:debug("~p received unknown event: ~p", [?MODULE, _Event]).






%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist(), list()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Attachements) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props, Attachements) || T <- To],
    ok;
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Attachements) ->
    From = props:get_value(<<"send_from">>, Props),
    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               } | Attachements
              ]
            },
    notify_util:send_email(From, To, Email),
    ok.








