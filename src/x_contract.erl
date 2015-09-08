%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_contract).
-export([check/2]).

-spec check(any(), fun((any())->boolean())) -> ok.
check(Val, Pred) ->
  try Pred(Val) of
    true -> ok;
    Other -> fail(Val, Pred, Other)
  catch E:R ->
          fail(Val, Pred, {E,R})
  end.

-spec fail(any(), fun((any())->boolean()), any()) -> no_return().
fail(Value, Predicate, PredicateResult) ->
  error({contract_failed, Value, {Predicate, PredicateResult}}).
