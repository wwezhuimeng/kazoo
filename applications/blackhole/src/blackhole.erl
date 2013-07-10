%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
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
    blackhole_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['sasl', 'crypto', 'whistle_amqp', 'cowboy', 'socketio']],
    'ok'.
