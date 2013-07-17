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

open(_Pid, _SId, _Opts) ->
    lager:debug("opening socket ~p", [_SId]),
    {'ok', []}.

close(Pid, _SId, _State) ->
    lager:debug("closing socket ~p", [_SId]),
    case blackhole_session:get_session_from_pid(Pid) of
    	[Session|_] ->
    		Info = blackhole_session:session_to_proplist(Session),
    		ConfId = proplists:get_value('conf_id', Info),
    		UserName = proplists:get_value('user_name', Info),
    		broadcast_event(ConfId, <<"user_disconnected">>, UserName);
		[] -> 'ok'
    end,
    blackhole_session:remove_session(Pid),
    'ok'.


%% Msg
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
	lager:debug("receive message ~p on socket ~p", [Message, _SId]),
    {'ok', State};
%% Custom Events
recv(Pid, _SId, {event, <<>>, Event, Message}, State) ->
   	handle_event(Event, Message, Pid),
    {'ok', State};
%% Catch all
recv(_Pid, _SId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, _SId]),
    {'ok', State}.

%% Connection Event
handle_event(<<"connection">>, Data, Pid) ->
	ConfId = wh_json:get_value(<<"conference_id">>, Data),
	UserName = wh_json:get_value(<<"user_name">>, Data),
	blackhole_session:add_session(ConfId, Pid, UserName),
	socketio_session:send_event(Pid, <<"connected">>, [UserName, ConfId]),
	broadcast_event(ConfId, <<"user_connected">>, UserName);
handle_event(<<"disconnection">>, _Data, Pid) ->
	case blackhole_session:get_session_from_pid(Pid) of
		[Session|_] ->
			Info = blackhole_session:session_to_proplist(Session),
			ConfId = proplists:get_value('conf_id', Info),
			UserName = proplists:get_value('user_name', Info),
			socketio_session:send_event(Pid, <<"disconnected">>, [UserName, ConfId]),
			broadcast_event(ConfId, <<"user_disconnected">>, UserName),
			blackhole_session:remove_session(Pid);
		[] -> 'ok'
    end;
handle_event(<<"connection_status">>, _Data, Pid) ->
	case blackhole_session:get_session_from_pid(Pid) of
		[_Session|_]->
			socketio_session:send_event(Pid, <<"connection_status">>, ['true']);
		[] ->
			socketio_session:send_event(Pid, <<"connection_status">>, ['false'])
    end;
%% Unknown Event
handle_event(Event, Data, Pid) ->
	lager:info("receive unknown event ~p", [Event]),
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










