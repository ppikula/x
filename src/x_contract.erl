-module(x_contract).
-export([check/2]).

-spec check(any(), fun((any())->boolean())) -> ok.
check(Val, Pred) ->
    try Pred(Val) of
        true -> ok;
        false -> error({contract_failed, Val});
        Other -> error({contract_failed, {Val, Other}})
    catch _:R ->
            error({contract_failed, {Val, R}})
    end.
