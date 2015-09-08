%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_contract_tests).
-include_lib("eunit/include/eunit.hrl").

contract_passes_with_ok_test_()->
  ?_assertEqual(ok,
                x_contract:check(1, fun is_number/1)).

contract_fails_when_pred_returns_false_test_()->
  Pred = fun is_number/1,
  ?_assertError({contract_failed, [], {Pred, false}},
                x_contract:check([], Pred)).

contract_fails_when_pred_returns_non_boolean_test_()->
  Pred = fun(_) -> ok end,
  ?_assertError({contract_failed, [], {Pred, ok}},
                x_contract:check([], Pred)).

contract_fails_when_pred_crashes_1_test_()->
  Pred = fun(x) -> true end,
  ?_assertError({contract_failed, y, {Pred, {error, function_clause}}},
                x_contract:check(y, Pred)).

contract_fails_when_pred_crashes_2_test_()->
  Pred = fun(L)-> length(L) == 1 end,
  ?_assertError({contract_failed, y, {Pred, {error, badarg}}},
                x_contract:check(y, Pred)).

contract_fails_when_pred_crashes_3_test_()->
  Pred = fun not_defined:anywhere/1,
  ?_assertError({contract_failed, y, {Pred, {error, undef}}},
                x_contract:check(y, Pred)).
