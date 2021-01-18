%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
-module(x_stream_features_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

create_empty_features_test_() ->
  Features = x_stream_features:new(),
  Expected = <<"<stream:features/>">>,
  ?_assertEqual(Expected, to_bin(Features)).

append_to_empty_features_list_test_() ->
  Features1 = x_stream_features:new(),
  Features2 = x_stream_features:add(Features1, <<"sm">>, x_ns:new(<<"test">>)),
  Expected = <<"<stream:features><sm xmlns='test'/></stream:features>">>,
  ?_assertEqual(Expected, to_bin(Features2)).

append_complex_feature_to_not_empty_list_test_() ->
  Features1 = x_stream_features:new(),
  Features2 = x_stream_features:add(Features1, <<"ms">>, x_ns:new(<<"hoax">>)),
  NS = x_ns:new(<<"urn:madeup">>),
  MechCdata = #xmlcdata{content = <<"SASL">>},
  Mechanism = #xmlel{name = <<"mechanism">>, children = [MechCdata]},
  Features3 = x_stream_features:add(Features2, <<"auth">>, NS, Mechanism),
  Expected = <<"<stream:features>"
                    "<ms xmlns='hoax'/>"
                    "<auth xmlns='urn:madeup'>"
                      "<mechanism>SASL</mechanism>"
                    "</auth>"
               "</stream:features>">>,
  ?_assertEqual(Expected, to_bin(Features3)).

should_not_add_the_same_feature_twice_test_() ->
  Expected = <<"<stream:features>"
                 "<ms xmlns='hoax'/>"
               "</stream:features>">>,
  Features1 = x_stream_features:new(),
  Features2 = x_stream_features:add(Features1, <<"ms">>,
                                    x_ns:new(<<"hoax">>)),
  %% try with simple
  Features3 = x_stream_features:add(Features2, <<"ms">>,
                                    x_ns:new(<<"hoax">>)),
  %% try with complex
  Features4 = x_stream_features:add(Features3, <<"ms">>,
                                    x_ns:new(<<"hoax">>),
                                    #xmlel{name = <<"simple">>}),

  ?_assertEqual(Expected, to_bin(Features4)).

construct_features_from_stanza_test_() ->
  Raw = <<"<stream:features xmlns:stream='http://etherx.jabber.org/streams'>"
                    "<ms xmlns='hoax'/>"
                    "<auth xmlns='urn:madeup'>"
                      "<mechanism>SASL</mechanism>"
                    "</auth>"
          "</stream:features>">>,
  {ok, Parsed} = exml:parse(Raw),
  Features = x_stream_features:new(Parsed),
  HoaxNS = x_ns:new(<<"hoax">>),
  AuthNS = x_ns:new(<<"urn:madeup">>),
  %% multiple properties
  [
   ?_assertEqual(true,  x_stream_features:has_feature(Features, AuthNS, <<"auth">>)),
   ?_assertEqual(false,  x_stream_features:has_feature(Features, HoaxNS, <<"auth">>)),
   ?_assertEqual(true,  x_stream_features:has_feature(Features, HoaxNS, <<"ms">>)),
   ?_assertEqual(false, x_stream_features:has_feature(Features, HoaxNS, <<"sm">>))
  ].

to_bin(El) ->
 exml:to_binary((x_exmlable:to_exmlel(El))).
