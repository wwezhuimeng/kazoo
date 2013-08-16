%%%-------------------------------------------------------------------
%%% @copyright (C) 2013 2600Hz Inc
%%% @doc
%%% Conference participant process
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(conference_history).

-include("conference.hrl").

-export([add_member/2]).
-export([del_member/2]).


add_member(Participant, Conference) ->
	AccountDb = wh_json:get_value([<<"Conference-Doc">>, <<"pvt_account_db">>], Conference),
	Id = wh_json:get_value(<<"Conference-Unique-ID">>, Participant),
	io:format("MARKER0 ~p~n", [Id]),
	CallId = wh_json:get_value(<<"Call-ID">>, Participant),
	case cfh_exist(AccountDb, Id) of
		{'error', 'not_found'} -> 
			Member = create_member(Participant, Conference),
			Participants = wh_json:set_value(CallId, Member, wh_json:new()),
			JObj = wh_json:set_values([{<<"participants">>, Participants}
									   ,{<<"_id">>, Id}
									   ,{<<"conference_id">>, wh_json:get_value(<<"Conference-ID">>, Conference)}
									  ], wh_json:new()),
			cfh_save(AccountDb, JObj);
		{'ok', Doc} ->
			Member = create_member(Participant, Conference),
			Participants = wh_json:set_value(CallId
											 ,Member
											 ,wh_json:get_value(<<"participants">>, Doc)),
			cfh_save(AccountDb, wh_json:set_value(<<"participants">>, Participants, Doc));
		{'error', _E} ->
			lager:error("error while opening doc ~p in ~p: ~p", [Id, AccountDb, _E])
	end.

del_member(Participant, Conference) ->
	AccountDb = wh_json:get_value([<<"Conference-Doc">>, <<"pvt_account_db">>], Conference),
	Id = wh_json:get_value(<<"Conference-Unique-ID">>, Participant),
	CallId = wh_json:get_value(<<"Call-ID">>, Participant),
	case cfh_exist(AccountDb, Id) of
		{'ok', Doc} ->
			Member = end_member(CallId, Doc),
			Participants = wh_json:set_value(CallId
											 ,Member
											 ,wh_json:get_value(<<"participants">>, Doc)),
			Saved = cfh_save(AccountDb, wh_json:set_value(<<"participants">>, Participants, Doc)),
			cfh_maybe_end(AccountDb, Saved);
		{'error', _E} ->
			lager:error("error while opening doc ~p in ~p: ~p", [Id, AccountDb, _E])
	end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_member(Participant, Conference) ->
	CfParticipant = wh_json:get_value(<<"Participant">>, Conference),
	Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	wh_json:set_values([{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, Participant)}
						,{<<"caller_id_name">>, wh_json:get_value(<<"Caller-ID-Name">>, Participant)}
						,{<<"caller_id_number">>, wh_json:get_value(<<"Caller-ID-Number">>, Participant)}
						,{<<"start_time">>, Timestamp}
					   ], CfParticipant).

end_member(Id, Member) ->
	Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	wh_json:set_value(<<"end_time">>
					  ,Timestamp
					  ,wh_json:get_value([<<"participants">>, Id], Member)).


cfh_maybe_end(AccountDb, JObj) ->
	End = wh_json:foldl(fun(_, V, Acc) ->
							case wh_json:get_value(<<"end_time">>, V) of
								'undefined' -> 'not_ended';
								_ -> Acc
							end
						end
						,'ended'
						,wh_json:get_value(<<"participants">>, JObj)),
	case End of
		'ended' -> cfh_end(AccountDb, JObj);
		'not_ended' -> 'ok'
	end.
	
cfh_end(AccountDb, JObj) ->
	Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	cfh_save(AccountDb, wh_json:set_value(<<"end_time">>, Timestamp, JObj)).


cfh_exist(AccountDb, Id) ->
	couch_mgr:open_doc(AccountDb, Id).

cfh_save(AccountDb, JObj) ->
	Routines = [fun(O) -> set_start_time(O) end
				,fun(O) -> set_pvt_type(O) end
				,fun(O) -> set_pvt_account_db(O, AccountDb) end
				,fun(O) -> set_pvt_created(O) end
				,fun(O) -> set_pvt_modified(O) end
			   ],
	JObj1 = lists:foldl(fun(F, O) -> F(O) end, JObj, Routines),
	cfh_save(AccountDb, JObj1, 3)


cfh_save(AccountDb, _, 0) ->
	Id = wh_json:get_value(<<"_id">>, JObj),
	lager:error("failed to conference_history ~p in ~p: too many attempts", [Id, AccountDb]);
cfh_save(AccountDb, JObj, Attempt) ->
	Id = wh_json:get_value(<<"_id">>, JObj),
	case couch_mgr:save_doc(AccountDb, JObj) of
		{'ok', Doc} ->
			lager:debug("new participant", []),
			Doc;
		{'error', 'conflict'} ->
			lager:info("failed to save conference_history ~p in ~p: conflict", [Id, AccountDb]),
			cfh_save(AccountDb, JObj, Attempt-1);
		{'error', _E} ->
			lager:error("failed to save conference_history ~p in ~p: ~p", [Id, AccountDb, _E]),
			'error'
	end.

set_start_time(JObj) ->
	case wh_json:get_value(<<"start_time">>, JObj) of
        'undefined' ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            wh_json:set_value(<<"start_time">>, Timestamp, JObj);
        _ ->
            JObj
    end.

set_pvt_type(JObj) ->
	wh_json:set_value(<<"pvt_type">>, <<"conference_history">>, JObj).

set_pvt_account_db(JObj, AccountDb) ->
   	case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        'undefined' ->
            wh_json:set_value(<<"pvt_account_db">>, AccountDb, JObj);
        _Else -> JObj
    end.

set_pvt_created(JObj) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        'undefined' ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            wh_json:set_value(<<"pvt_created">>, Timestamp, JObj);
        _ ->
            JObj
    end.

set_pvt_modified(JObj) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    wh_json:set_value(<<"pvt_modified">>, Timestamp, JObj).

