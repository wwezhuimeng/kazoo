%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_ws).

-export([open/3, recv/4, close/3]).

-record(session_state, {}).

open(Pid, Sid, _Opts) ->
    io:format("open ~p ~p~n", [Pid, Sid]),
    {ok, #session_state{}}.

recv(Pid, Sid, {event, <<>>, Event, Message}, SessionState = #session_state{}) ->
    io:format("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    socketio_session:send_event(Pid, Event, Message),
    {ok, SessionState}.




close(Pid, Sid, _SessionState = #session_state{}) ->
    io:format("close ~p ~p~n", [Pid, Sid]),
    ok.





