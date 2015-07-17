-module(x_contract_tests).
-include_lib("eunit/include/eunit.hrl").

contract_passes_with_ok_test_()->
    ?_assertEqual(ok,
                 x_contract:check(1, fun is_number/1)).

contract_fails_when_pred_returns_false_test_()->
    ?_assertError({contract_failed, []},
                  x_contract:check([], fun is_number/1)).

contract_fails_when_pred_returns_non_boolean_test_()->
    ?_assertError({contract_failed, {[], ok}},
                  x_contract:check([], fun(_) -> ok end)).

contract_fails_when_pred_crashes_1_test_()->
    ?_assertError({contract_failed, {y, function_clause}},
                  x_contract:check(y, fun(x) -> true end)).

contract_fails_when_pred_crashes_2_test_()->
    ?_assertError({contract_failed, {y, badarg}},
                  x_contract:check(y, fun(L)-> length(L) == 1 end)).

contract_fails_when_pred_crashes_3_test_()->
    ?_assertError({contract_failed, {y, undef}},
                  x_contract:check(y, fun not_defined:anywhere/1)).
