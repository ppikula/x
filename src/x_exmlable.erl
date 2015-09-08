%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_exmlable).

-include_lib("exml/include/exml_stream.hrl").

-export([to_exmlel/1]).

%% xmlstreamelement - both xmlel and streams start/stop
-callback to_exmlel(any()) -> xmlstreamelement().

-spec to_exmlel(tuple()) -> xmlstreamelement().
to_exmlel(Xmlable) ->
  (x_implementation:module(Xmlable)):to_exmlel(Xmlable).
