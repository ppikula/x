%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_stream_tests).
-include_lib("eunit/include/eunit.hrl").

stream_start_test_() ->
  Expected  = <<"<stream:stream to='localhost' xml:lang='en' version='1.0'>">>,
  Header = x_stream_start:new(x_jid:from_bin(<<"localhost">>), en, 1.0),
  ?_assertEqual(Expected, exml:to_binary((x_exmlable:to_exmlel(Header)))).

stream_end_test_() ->
  Expected = <<"</stream:stream>">>,
  Trailer = x_stream_end:new(),
  ?_assertEqual(Expected, exml:to_binary(x_exmlable:to_exmlel(Trailer))).

is_stream_start_accepts_valid_stream_start_test_() ->
  S = x_stream_start:new(x_jid:from_bin(<<"localhost10.io">>), pl, 1.0),
  ?_assertEqual(true, x_stream_start:is_stream_start(S)).

is_stream_end_accepts_valid_stream_end_test_() ->
  ?_assertEqual(true, x_stream_end:is_stream_end(x_stream_end:new())).

%% it is more like smoke test - I think it is a good candidate for proper
is_stream_start_reject_invalid_terms_test_() ->
  ?_assertEqual(false, x_stream_start:is_stream_start((x_stream_end:new()))).

is_stream_end_reject_invalid_terms_test_() ->
  S = x_stream_start:new(x_jid:from_bin(<<"localhost44.io">>), pl, 2.0),
  ?_assertEqual(false, x_stream_end:is_stream_end(S)).
