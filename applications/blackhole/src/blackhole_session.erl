%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_session).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-export([add_session/2
         ,remove_session/1
         ,remove_session/2
         ,remove_conference/1
        ]).

-export([get_name/0]).

-define(SESSIONS, 'blackhole_session').


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Sessions = ets:new(?SESSIONS, ['named_table'
                                   ,'duplicate_bag'
                                   ,'protected'
                                  ]),
    {ok, Sessions}.


get_name() ->
    gen_server:call(?MODULE, 'get_name').

add_session(ConfId, Pid) ->
    gen_server:cast(?MODULE, {'add_session', ConfId, Pid}).


remove_session(Pid) ->
    gen_server:cast(?MODULE, {'remove_session', Pid}).

remove_session(ConfId, Pid) ->
    gen_server:cast(?MODULE, {'remove_session', ConfId, Pid}).

remove_conference(ConfId) ->
    gen_server:cast(?MODULE, {'remove_conference', ConfId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('get_name', _From, State) ->
    {'reply', ?SESSIONS, State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'add_session', ConfId, Pid}, State) ->
    ets:insert(?SESSIONS, {ConfId, Pid}),
    {'noreply', State};
handle_cast({'remove_conference', ConfId}, State) ->
    ets:delete(?SESSIONS, ConfId),
    {'noreply', State};
handle_cast({'remove_session', Pid}, State) ->
    ets:match_delete(?SESSIONS, {'_', Pid}),
    {'noreply', State};
handle_cast({'remove_session', ConfId, Pid}, State) ->
    ets:delete_object(?SESSIONS, {ConfId, Pid}),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
