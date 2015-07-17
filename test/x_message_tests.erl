-module(x_message_tests).
-include_lib("eunit/include/eunit.hrl").

is_message_accepts_messages_test_()->
  ?_assert(x_message:is_message(x_message:new
   (x_jid:from_bin(<<"from@example.com">>), 
    x_jid:from_bin(<<"to@example.com">>),
    "hello"))).

is_message_rejects_non_messages_test_()->
  ?_assertEqual(false, x_message:is_message(not_message)).

a_message_implements_Addressable_from_test_() ->
  From = x_jid:from_bin(<<"sender@foo.com">>),
  To = x_jid:from_bin(<<"recipient@foo.com">>),
  ?_assert(x_jid:is_same_user(From, 
                x_Addressable:from(x_message:new(From, To,<<"!">>)))).

a_message_implements_Addressable_to_test_() ->
  From = x_jid:from_bin(<<"sender@bar.com">>),
  To = x_jid:from_bin(<<"recipient@bar.com">>),
  ?_assert(x_jid:is_same_user(To, 
                x_Addressable:to(x_message:new(From, To,<<"ole!">>)))).


