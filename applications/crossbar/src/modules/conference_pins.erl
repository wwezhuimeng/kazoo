%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Conferences module
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(conference_pins).

-export([get/2]).
-export([unique/2]).

-include("../crossbar.hrl").

-define(CONF_PIN_LIST, <<"conferences/listing_by_pin">>).
-define(CONF_PIN_DOC, <<"conferences_pins">>).
-define(CONF_PIN_NUMBER, 50).
-define(CONF_PIN_NUMBER_MAX, 500).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary(), integer()) -> [] | ne_binaries().
get(AcctDb, Number) ->
    case get_pins_doc(AcctDb) of
    	'error' -> [];
    	'not_found' ->
    		'ok' = put_pins_doc(AcctDb),
    		get(AcctDb, Number);
    	Doc ->
    		Pins = wh_json:get_value(<<"pins">>, Doc),
    		maybe_generate_pins(AcctDb, Pins, Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unique(ne_binary(), ne_binaries()) -> [] | ne_binaries().
-spec unique(ne_binary(), ne_binaries(), ne_binaries()) -> [] | ne_binaries().
unique(AcctDb, Pins) ->
    unique(AcctDb, Pins, get_used_pins(AcctDb)).

unique(_AcctDb, _Pins, 'error') ->
	[];
unique(AcctDb, Pins, UsedPins) ->
    {Miss, NPins} = lists:foldl(fun(Pin, {Missing, Acc}) ->
				                    case lists:member(Pin, UsedPins) of
				                        'false' ->
				                            {Missing, [Pin|Acc]};
				                        'true' ->
				                            {Missing+1, Acc}
				                    end
				                end
				                ,{0, []}
				                ,Pins
				                ),
    case Miss of
        0 -> NPins;
        N ->
            lists:merge(generate_pins(AcctDb, N, []), NPins)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_generate_pins(ne_binary(), ne_binaries(), integer()) -> [] | ne_binaries().
maybe_generate_pins(AcctDb, Pins, Number) ->
	Length = erlang:length(Pins),
	case Number >  Length of
		'true' ->
			NPins = generate_pins(AcctDb, (Number-Length), Pins),
            'ok' = post_pins_doc(AcctDb, NPins),
            get(AcctDb, Number);
		'false' ->
			{L1, L2} = lists:split(Number, Pins),
			'ok' = post_pins_doc(AcctDb, L2),
			regenerate_pins(AcctDb, L2, L1),
			L1
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec regenerate_pins(ne_binary(), ne_binaries(), ne_binaries()) -> 'ok'.
regenerate_pins(AcctDb, Pins, UsedPins) ->
	spawn(fun() ->
			case erlang:length(Pins) < 5 of
				'true' ->
					lager:info("creating new Pins for ~p", [AcctDb]),
					NPins = generate_pins(AcctDb, ?CONF_PIN_NUMBER, UsedPins),
					post_pins_doc(AcctDb, lists:merge(Pins, NPins));
				'false' -> ok
			end
		  end),
	'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_pins_doc(ne_binary()) -> 'error' | 'not_found' | wh_json:object().
get_pins_doc(AcctDb) -> 
	case couch_mgr:open_doc(AcctDb, ?CONF_PIN_DOC) of
        {'ok', Doc} ->
            Doc;
        {'error', 'not_found'} ->
            lager:info("missing doc ~p in ~p creating...", [?CONF_PIN_DOC, AcctDb]),
            'not_found';
        {'error', _E} ->
            lager:error("failed to open ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put_pins_doc(ne_binary()) -> 'error' | 'ok'.
put_pins_doc(AcctDb) ->
    JObj  = wh_json:set_values([{<<"_id">>, ?CONF_PIN_DOC}
                               ,{<<"pins">>, generate_pins(AcctDb, ?CONF_PIN_NUMBER, [])}
                              ]
                              ,wh_json:new()),
    case couch_mgr:save_doc(AcctDb, JObj) of
        {'ok', _} ->
            lager:info("~p created in ~p", [?CONF_PIN_DOC, AcctDb]),
            'ok';
        {'error', _E} ->
            lager:error("failed to create ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post_pins_doc(ne_binary(), ne_binaries()) -> 'error' | 'ok'.
post_pins_doc(AcctDb, Pins) ->
    UpdateProps = [{<<"pins">>, Pins}],
    case couch_mgr:update_doc(AcctDb, ?CONF_PIN_DOC, UpdateProps) of
        {'ok', _} ->
            lager:debug("~p updated in ~p", [?CONF_PIN_DOC, AcctDb]),
            'ok';
        {'error', _E} ->
            lager:error("failed to update ~p in ~p, reason: ~p", [?CONF_PIN_DOC, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec generate_pins(ne_binary(), integer(), ne_binaries()) -> ne_binaries().
-spec generate_pins(ne_binary(), integer(), ne_binaries(), ne_binaries()) -> ne_binaries().
generate_pins(AcctDb, Number, UsedPins) ->
	generate_pins(AcctDb, Number, UsedPins, []).

generate_pins(AcctDb, 0, _UsedPins, Acc) ->
    unique(AcctDb, Acc);
generate_pins(AcctDb, Number, UsedPins, Acc) ->
    Pin = create_pin(),
    case {lists:member(Pin, Acc)
    	  ,lists:member(Pin, UsedPins)
    	 }
    of
        {'false', 'false'} ->
            generate_pins(AcctDb, Number-1, UsedPins, [Pin|Acc]);
        _ ->
            generate_pins(AcctDb, Number, UsedPins, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_used_pins(ne_binary()) -> 'error' | ne_binaries().
get_used_pins(AcctDb) ->
    case couch_mgr:get_all_results(AcctDb, ?CONF_PIN_LIST) of
        {'ok', JObjs} ->
            ViewPins = couch_mgr:get_result_keys(JObjs),
            case get_pins_doc(AcctDb) of
            	'error' -> 'error';
            	'not_found' -> ViewPins;
            	Doc ->
            		lists:merge(ViewPins, wh_json:get_value(<<"pins">>, Doc))
            end;
        {'error', _E} ->
            lager:error("failed to check view ~p in ~p, reason: ~p", [?CONF_PIN_LIST, AcctDb, _E]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_pin() -> ne_binary().
create_pin() ->
    Pin =  wh_util:to_binary(crypto:rand_uniform(0, 999999)),
    Length = erlang:length(erlang:binary_to_list(Pin)),
    case  Length < 6 of
        'true' ->
            Prefix = binary:copy(<<"0">>, 6-Length),
            <<Prefix/binary, Pin/binary>>;
        'false' -> Pin
    end.




