%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/socket.io/1/[...]"
				,'socketio_handler'
				,[socketio_session:configure([{'heartbeat', 5000}
											  ,{'heartbeat_timeout', 30000}
											  ,{'session_timeout', 30000}
											  ,{'callback', 'blackhole_ws'}
											  ,{'protocol', 'socketio_data_protocol'}
											 ])]}
			  ]
        }
     ]),
	{ok, _} = cowboy:start_http('socketio_http_listener', 100, [{'port', 5555}],
									[{'env', [{'dispatch', Dispatch}]}]),
	blackhole:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) ->
	cowboy:stop_listener('blackhole'),
	blackhole:stop().
