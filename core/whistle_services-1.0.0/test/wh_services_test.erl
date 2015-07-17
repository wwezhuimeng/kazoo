%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services_test).

-include_lib("eunit/include/eunit.hrl").

-define(CAT, <<"phone_numbers">>).
-define(ITEM, <<"did_us">>).

-define(USER_CAT, <<"users">>).
-define(USER_ITEM, <<"user">>).

-record(state, {service_plan_jobj :: kzd_service_plan:plan()
                ,services :: wh_services:services()
                ,services_jobj :: wh_json:object()
                ,account_plan :: kzd_service_plan:plan()
                ,schema_loader_fun :: fun((binary()) -> {'ok', wh_json:object()})
               }).

services_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun service_plan_schema_check/1
       ,fun services_json_to_record/1
       ,fun services_record_to_json/1
       ,fun service_plan_json_to_plans/1
       ,fun increase_quantities/1
       ,fun user_additions/1
      ]
    }.

init() ->
    lists:foldl(fun init_fold/2
                ,#state{}
                ,[fun read_service_plan/1
                  ,fun read_services/1
                  ,fun build_schema_loader_fun/1
                 ]).

init_fold(F, State) ->
    F(State).

stop(#state{}=_State) ->
    'ok'.

read_service_plan(State) ->
    ServicePlan1 = filename:join([priv_dir(), "example_service_plan_1.json"]),
    JObj = read_json(ServicePlan1),

    State#state{service_plan_jobj=JObj}.

read_services(#state{service_plan_jobj=ServicePlan}=State) ->
    ServicesPath = filename:join([priv_dir(), "example_account_services.json"]),

    ServicesJObj = read_json(ServicesPath),
    ServicesWithPlanJObj = kzd_services:set_plan_doc(ServicesJObj, wh_doc:id(ServicePlan), ServicePlan),

    Services = wh_services:from_service_json(ServicesWithPlanJObj, 'false'),

    Overrides = kzd_services:plan_overrides(ServicesWithPlanJObj, wh_doc:id(ServicePlan)),
    AccountPlan = kzd_service_plan:merge_overrides(ServicePlan, Overrides),

    State#state{services_jobj=ServicesWithPlanJObj
                ,services=Services
                ,account_plan=AccountPlan
               }.

build_schema_loader_fun(#state{}=State) ->
    SchemaIds = [<<"service_plans">>, <<"service_plan">>, <<"service_plan_items">>, <<"bookkeepers">>],
    Schemas = [{Id, load_schema(Id)} || Id <- SchemaIds],
    State#state{schema_loader_fun=
                    fun(Name) -> {'ok', props:get_value(Name, Schemas)} end
               }.

load_schema(Id) ->
    CBPriv = code:priv_dir('crossbar'),
    Path = filename:join([CBPriv, "couchdb", "schemas", <<Id/binary, ".json">>]),
    read_json(Path).

priv_dir() ->
    case code:priv_dir('whistle_services') of
        Path when is_list(Path) -> Path;
        _ ->
            {'ok', AppDir} = file:get_cwd(),
            filename:join([AppDir, "priv"])
    end.

read_json(Path) ->
    {'ok', JSON} = file:read_file(Path),
    wh_json:decode(JSON).

services_json_to_record(#state{services=Services
                               ,services_jobj=JObj
                              }) ->
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

services_record_to_json(#state{services=Services}) ->
    JObj = wh_services:to_json(Services),
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

quantity_checks(Services, JObj) ->
    {Tests, _} = wh_json:foldl(fun category_checks/3
                               ,{[], Services}
                               ,kzd_services:quantities(JObj)
                              ),
    Tests.

category_checks(Category, CategoryJObj, Acc) ->
    wh_json:foldl(fun(K, V, Acc1) ->
                          item_checks(Category, K, V, Acc1)
                  end
                  ,Acc
                  ,CategoryJObj
                 ).

item_checks(Category, Item, Quantity, {Tests, Services}) ->
    {[item_check(Category, Item, Quantity, Services) | Tests]
     ,Services
    }.

item_check(Category, Item, Quantity, Services) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, Quantity]))
     ,?_assertEqual(Quantity, wh_services:quantity(Category, Item, Services))
    }.

service_plan_schema_check(#state{service_plan_jobj=ServicePlan
                                 ,schema_loader_fun=F
                                }) ->
    {"checking service plan fixture against schema"
     ,try wh_json_schema:validate(<<"service_plans">>, ServicePlan, [{'schema_loader_fun', F}]) of
          {'ok', _} -> ?_assert('true');
          {'error', _E} ->
              ?debugFmt("schema errors: ~p~n", [_E]),
              ?_assert('false')
      catch
          'error':_E ->
              ST = erlang:get_stacktrace(),
              ?debugFmt("error: ~p~n~p~n", [_E, ST]),
              ?_assert('false')
      end
    }.

service_plan_json_to_plans(#state{service_plan_jobj=ServicePlan
                                  ,account_plan=AccountPlan
                                 }) ->

    [{"Verify plan from file matches services plan"
      ,?_assertEqual(wh_doc:account_id(ServicePlan)
                     ,kzd_service_plan:account_id(AccountPlan)
                    )
     }
     ,{"Verify cumulative discount rate from service plan"
       ,?_assertEqual(0.5, rate(cumulative_discount(did_us_item(ServicePlan))))
      }
     ,{"Verify cumulative discount rate was overridden"
       ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(AccountPlan))))
      }
    ].

did_us_item(Plan) ->
    kzd_service_plan:item(Plan, ?CAT, ?ITEM).

cumulative_discount(Item) ->
    kzd_item_plan:cumulative_discount(Item).

rate(JObj) ->
    wh_json:get_float_value(<<"rate">>, JObj).

increase_quantities(#state{services=Services}=_State) ->
    ItemQuantity = wh_services:quantity(?CAT, ?ITEM, Services),

    random:seed(os:timestamp()),
    Increment = random:uniform(10),

    UpdatedServices = wh_services:update(?CAT, ?ITEM, ItemQuantity+Increment, Services),

    UpdatedItemQuantity = wh_services:quantity(?CAT, ?ITEM, UpdatedServices),

    DiffItemQuantity = wh_services:diff_quantity(?CAT, ?ITEM, UpdatedServices),

    [{"Verify base quantity on the services doc"
      ,?_assertEqual(9, ItemQuantity)
     }
     ,{"Verify incrementing the quantity"
       ,?_assertEqual(ItemQuantity+Increment, UpdatedItemQuantity)
      }
     ,{"Verify the diff of the quantity"
       ,?_assertEqual(Increment, DiffItemQuantity)
      }
     | category_quantities(Services, UpdatedServices, Increment)
    ].

category_quantities(CurrentServices, UpdatedServices, Increment) ->
    CategoryQuantity = wh_services:category_quantity(?CAT, CurrentServices),
    UpdatedCategoryQuantity = wh_services:category_quantity(?CAT, UpdatedServices),

    TollFreeQuantity = wh_services:quantity(?CAT, <<"toll_free">>, UpdatedServices),
    DIDUSQuantity = wh_services:quantity(?CAT, ?ITEM, UpdatedServices),

    MinusTollFree = wh_services:category_quantity(?CAT, [<<"toll_free">>], UpdatedServices),
    MinusDIDUS = wh_services:category_quantity(?CAT, [?ITEM], UpdatedServices),

    [{"Verify base category quantities"
      ,?_assertEqual(10, CategoryQuantity)
     }
     ,{"Verify updated category quantities"
       ,?_assertEqual(CategoryQuantity + Increment, UpdatedCategoryQuantity)
      }
     ,{"Verify updated category quantities minus toll_free numbers"
       ,?_assertEqual(MinusTollFree, UpdatedCategoryQuantity-TollFreeQuantity)
      }
     ,{"Verify updated category quantities minus did_us numbers"
       ,?_assertEqual(MinusDIDUS, UpdatedCategoryQuantity-DIDUSQuantity)
      }
    ].

user_additions(#state{services=Services
                      ,service_plan_jobj=_ServicePlan
                     }) ->
    ItemQuantity = wh_services:quantity(?USER_CAT, ?USER_ITEM, Services),
    UpdatedServices = wh_services:update(?USER_CAT, ?USER_ITEM, ItemQuantity+2, Services),

    ServiceJObj = wh_services:to_json(Services),
    ServicePlans = wh_service_plans:from_service_json(ServiceJObj),
    UpdatedServiceJObj = wh_services:to_json(UpdatedServices),

    ExistingItems = wh_service_plans:create_items(ServiceJObj, ServicePlans),
    UpdatedItems = wh_service_plans:create_items(UpdatedServiceJObj, ServicePlans),
    Changed = wh_service_items:get_updated_items(UpdatedItems, ExistingItems),

    ActivationCharges = wh_services:dry_run_activation_charges(UpdatedServices),
    PlanCharges = wh_service_items:public_json(Changed),

    DryRun = wh_services:calculate_transactions_charges(PlanCharges, ActivationCharges),
    ?debugFmt("dry run: ~p~n", [DryRun]),

    [{"Verify quantity changed"
      ,?_assertEqual(2, wh_json:get_integer_value([?USER_CAT, ?USER_ITEM, <<"quantity">>], DryRun))
     }
     ,{"Verify rate"
       ,?_assertEqual(5.0, wh_json:get_float_value([?USER_CAT, ?USER_ITEM, <<"rate">>], DryRun))
      }
    ].
