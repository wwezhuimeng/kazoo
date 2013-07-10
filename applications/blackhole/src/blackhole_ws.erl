%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_ws).

-export([open/3
		 ,recv/4
		 ,close/3
		]).


open(Pid, _SId, _Opts) ->
    io:format("open ~p~n", [Pid]),
    {'ok', []}.

close(Pid, _SId, _State) ->
    io:format("close ~p~n", [Pid]),
    case ets:match_object(blackhole_session:get_name(), {'_', Pid}) of
    	[{ConfId, _}|_] ->
    		broadcast_event(ConfId, <<"user_disconnected">>, [{}]);
    	_ -> 'ok'
    end,
    blackhole_session:remove_session(Pid),
    'ok'.


%% Msg
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
	io:format("recv msg ~p~n", [Message]),
    %%socketio_session:send_obj(Pid, Json),
    {'ok', State};
%% Custom Events
recv(Pid, _SId, {event, <<>>, Event, Message}, State) ->
    io:format("recv event ~p ~p~n", [Event, Message]),
   	handle_event(Event, Message, Pid),
    {'ok', State};
%% Catch all
recv(_Pid, _SId, Message, State) ->
    io:format("recv unknow message ~p~n", [Message]),
    {'ok', State}.


handle_event(<<"connection">>, Data, Pid) ->
	ConfId = proplists:get_value(<<"conf_name">>, Data),
	blackhole_session:add_session(ConfId, Pid),
	socketio_session:send_event(Pid, <<"connected">>, [{<<"conf_name">>, ConfId}]),
	broadcast_event(ConfId, <<"user_connected">>, Data);
handle_event(_Event, _Data, Pid) ->
	io:format("Got unknown event ~p~n", [_Event]),
	socketio_session:send_event(Pid, <<"unknown_event">>, [{}]).

broadcast_event(ConfId, Event, Data) ->
	Sessions = ets:lookup(blackhole_session:get_name(), ConfId),
	lists:foldl(
		fun({_, Pid}, _) ->
			socketio_session:send_event(Pid, Event, Data)
		end
		,'ok'
		,Sessions
	).










