%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_message_tests).
-include_lib("eunit/include/eunit.hrl").

is_message_accepts_messages_test_()->
  ?_assert(x_message:is_message
             (x_message:new
                (x_jid:from_bin(<<"from@example.com">>),
                 x_jid:from_bin(<<"to@example.com">>),
                 "hello"))).

is_message_rejects_non_messages_test_()->
  ?_assertEqual(false, x_message:is_message(not_message)).

from_must_contain_a_jid_test_() ->
  ?_assertError
     ({contract_failed, notjid, _},
      x_message:new(notjid, x_jid:from_bin(<<"to@x.y">>), <<"a">>)).

to_must_contain_a_jid_test_() ->
  ?_assertError
     ({contract_failed, {a,tuple}, _},
      x_message:new(x_jid:from_bin(<<"to@x.y">>), {a,tuple}, <<"a">>)).

a_message_implements_Addressable_from_test_() ->
  From = x_jid:from_bin(<<"sender@foo.com">>),
  To = x_jid:from_bin(<<"recipient@foo.com">>),
  ?_assert(x_jid:is_same_user
             (From,
              x_addressable:from(x_message:new(From, To,<<"!">>)))).

a_message_implements_Addressable_to_test_() ->
  From = x_jid:from_bin(<<"sender@bar.com">>),
  To = x_jid:from_bin(<<"recipient@bar.com">>),
  ?_assert(x_jid:is_same_user
             (To,
              x_addressable:to(x_message:new(From, To,<<"ole!">>)))).
