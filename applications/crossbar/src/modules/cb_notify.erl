%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600hz
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_notify).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/1, allowed_methods/2
         ,resource_exists/1, resource_exists/2
         ,content_types_provided/3
         ,content_types_accepted/2
         ,validate/2 ,validate/3
         ,put/2
         ,post/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"notify/crossbar_listing">>).


-define(NOTIFY, <<"notify">>).
-define(CONF_INVITE, <<"conference_invite">>).
-define(CONF_UPDATE, <<"conference_update">>).
-define(CONF_SUM, <<"conference_summary">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.notify">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.notify">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.notify">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.notify">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.notify">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.notify">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.notify">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.notify">>, ?MODULE, 'post').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods() | [].
-spec allowed_methods(path_token(), path_token()) -> http_methods() | [].
allowed_methods(?CONF_INVITE) ->
    [?HTTP_PUT, ?HTTP_POST];
allowed_methods(?CONF_UPDATE) ->
    [?HTTP_PUT, ?HTTP_POST];
allowed_methods(?CONF_SUM) ->
    [?HTTP_PUT, ?HTTP_POST].

allowed_methods(?CONF_INVITE, _) ->
    [?HTTP_GET];
allowed_methods(?CONF_UPDATE, _) ->
    [?HTTP_GET];
allowed_methods(?CONF_SUM, _) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /notify => []
%%    /notify/foo => [<<"foo">>]
%%    /notify/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?CONF_INVITE) -> 'true';
resource_exists(?CONF_UPDATE) -> 'true';
resource_exists(?CONF_SUM) -> 'true'.

resource_exists(?CONF_INVITE, _) -> 'true';
resource_exists(?CONF_UPDATE, _) -> 'true';
resource_exists(?CONF_SUM, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(#cb_context{}=Context, _) ->
    CTPs = [{'from_binary', [{<<"text">>, <<"html">>}
                           ,{<<"text">>, <<"plain">>}
                          ]}
           ],
    Context#cb_context{content_types_accepted=CTPs}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(#cb_context{}=Context, Name, Ext) ->
    DocId = <<?NOTIFY/binary, "_", Name/binary>>,
    AttName = <<Name/binary, ".", Ext/binary>>,
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            CT = wh_json:get_value([<<"_attachments">>, AttName, <<"content_type">>], JObj),
            [Type, SubType] = binary:split(CT, <<"/">>),
            Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /notify mights load a list of skel objects
%% /notify/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Name, Type) ->
    read(Context, Name, Type).
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _) ->
    cb_context:set_resp_status(Context, 'success');
validate(#cb_context{req_verb = ?HTTP_POST}=Context, _) ->
    cb_context:set_resp_status(Context, 'success').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(#cb_context{req_files=[{_, FileObj}]}=Context, Name) ->
    DocId = <<?NOTIFY/binary, "_", Name/binary>>,
    Doc = wh_json:set_value(<<"_id">>, DocId, wh_json:new()),
    crossbar_doc:save(Context#cb_context{doc=Doc}),
    Content = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    [_, Type] = binary:split(CT, <<"/">>),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, <<Name/binary, ".", Type/binary>>, Content, Context, Opts).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(#cb_context{req_files=[{_, FileObj}]}=Context, Name) ->
    DocId = <<?NOTIFY/binary, "_", Name/binary>>,
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    [_, Type] = binary:split(CT, <<"/">>),
    AttName = <<Name/binary, ".", Type/binary>>,
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, AttName, Contents, Context, Opts).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read(Context, Name, Type) ->
    DocId = <<?NOTIFY/binary, "_", Name/binary>>,
    AttName = <<Name/binary, ".", Type/binary>>,
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            Attachments = wh_json:get_value([<<"_attachments">>], JObj, []),
            case lists:member(AttName, wh_json:get_keys(Attachments)) of
                'true' ->
                    lists:foldl(fun({K, V}, C) ->
                                    cb_context:add_resp_header(K, V, C)
                                end
                                ,crossbar_doc:load_attachment(JObj, AttName, Context)
                                ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttName/binary>>}
                                  ,{<<"Content-Type">>, wh_json:get_value([AttName, <<"content_type">>], Attachments)}
                                  ,{<<"Content-Length">>, wh_json:get_value([AttName, <<"length">>], Attachments)}
                                 ]);
                'false' ->
                    crossbar_util:response_bad_identifier(AttName, Context)
            end;
        Context1 -> Context1
    end.



