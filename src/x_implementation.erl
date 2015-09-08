%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_implementation).

-export([module/1]).

-spec module(tuple()) -> module().
module(Record) when is_tuple(Record) ->
  element(1, Record).
