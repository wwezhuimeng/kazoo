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

-export([broadcast_event/3]).

open(Pid, _SId, _Opts) ->
    io:format("open ~p~n", [Pid]),
    {'ok', []}.

close(Pid, _SId, _State) ->
    io:format("close ~p~n", [Pid]),
    case blackhole_session:get_session_from_pid(Pid) of
    	[Session|_] ->
    		Info = blackhole_session:session_to_proplist(Session),
    		ConfId = proplists:get_value('conf_id', Info),
    		UserName = proplists:get_value('user_name', Info),
    		broadcast_event(ConfId, <<"user_disconnected">>, [{<<"user_name">>, UserName}]);
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

%% Connection Event
handle_event(<<"connection">>, Data, Pid) ->
	ConfId = wh_json:get_value(<<"conf_name">>, Data),
	UserName = wh_json:get_value(<<"user_name">>, Data),
	blackhole_session:add_session(ConfId, Pid, UserName),
	socketio_session:send_event(Pid, <<"connected">>, wh_json:from_list([{<<"conf_name">>, ConfId}])),
	broadcast_event(ConfId, <<"user_connected">>, Data);
%% Unknown Event
handle_event(Event, Data, Pid) ->
	io:format("Got unknown event ~p~n", [Event]),
	Unknown = [{<<"event">>, Event}
			   ,{<<"data">>, Data}
			  ],
	socketio_session:send_event(Pid, <<"unknown_event">>, Unknown).

broadcast_event(ConfId, Event, Data) ->
	Sessions = blackhole_session:get_sessions(ConfId),
	lists:foldl(
		fun(Session, _) ->
			Pid = proplists:get_value('pid', blackhole_session:session_to_proplist(Session)),
			socketio_session:send_event(Pid, Event, Data)
		end
		,'ok'
		,Sessions
	).










