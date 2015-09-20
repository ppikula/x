%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
-module(x_ns_tests).
-include_lib("eunit/include/eunit.hrl").

to_attr_test_() ->
  NS = x_ns:new(<<"urn:pawel:0">>),
  ?_assertEqual({<<"xmlns">>,<<"urn:pawel:0">>}, x_ns:to_attr(NS)).

equality_test_() ->
  N1 = x_ns:new(<<"urn:pawel:0">>),
  N2 = x_ns:new(<<"urn:pawel:0">>),
  ?_assertEqual(true, x_ns:equals(N1, N2)).

pred_test_() ->
  ?_assertEqual(false, x_ns:is_ns(undefined)).

inequality_test_() ->
  N1 = x_ns:new(<<"urn:pawel:0">>),
  N2 = x_ns:new(<<"urn:pawel:1">>),
  ?_assertEqual(false, x_ns:equals(N1, N2)).

positive_subns_test_() ->
  N1 = x_ns:new(<<"urn:pawel:0">>),
  N2 = x_ns:new(<<"urn:pawel:0#admin">>),
  ?_assertEqual(true, x_ns:is_subnamespace_of(N2, N1)).

negative_subns_test_() ->
  N1 = x_ns:new(<<"urn:pawel:0">>),
  N2 = x_ns:new(<<"urn:pawel:0#admin">>),
  ?_assertEqual(false, x_ns:is_subnamespace_of(N1, N2)).
