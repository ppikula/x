%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_jid_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

from_bin_to_bin_test_() ->
  B = <<"hello@world.com">>,
  ?_assertEqual(B,
                x_jid:to_bin(x_jid:from_bin(B))).

from_bin_to_bin_w_resource_test_() ->
  B = <<"hello@world.com/pc">>,
  ?_assertEqual(B,
                x_jid:to_bin(x_jid:from_bin(B))).

from_bin_to_bin_host_only_test_() ->
  B = <<"localhost">>,
  ?_assertEqual(B,
                x_jid:to_bin(x_jid:from_bin(B))).

from_bin_at_without_user_test_() ->
  B = <<"@localhost">>,
  ?_assertThrow(malformed_jid,x_jid:from_bin(B)).

from_bin_multiple_at_test_() ->
  B = <<"pawel@pawel@localhost">>,
  ?_assertThrow(malformed_jid, x_jid:from_bin(B)).

from_bin_multiple_slashes_test_() ->
  B = <<"pawel@kanku/res1/res2">>,
  ?_assertThrow(malformed_jid, x_jid:from_bin(B)).

to_bin_from_bin_test_() ->
  Jid = x_jid:new(<<"super">>,<<"hero.com">>),
  ?_assertEqual(Jid,
                x_jid:from_bin(x_jid:to_bin(Jid))).

to_bin_from_bin_w_resource_test_() ->
  Jid = x_jid:new(<<"super">>,<<"hero.com">>,<<"batmobile">>),
  ?_assertEqual(Jid,
                x_jid:from_bin(x_jid:to_bin(Jid))).

to_bin_from_bin_hostonly_test_() ->
  Jid = x_jid:new(<<>>,<<"hero.com">>,<<"batmobile">>),
  ?_assertEqual(Jid,
                x_jid:from_bin(x_jid:to_bin(Jid))).

same_resource_is_full_equality_test_() ->
  J = x_jid:from_bin(<<"a@b.c/resource-1">>),
  ?_assert(x_jid:is_same_resource(J,J)).

same_resource_is_false_for_same_user_different_resource_test_() ->
  R1 = x_jid:from_bin(<<"a@b.c/resource-10">>),
  R2 = x_jid:from_bin(<<"a@b.c/resource-99">>),
  ?_assertNot(x_jid:is_same_resource(R1,R2)).

same_resource_is_false_for_same_user_different_domain_test_() ->
  R1 = x_jid:from_bin(<<"a@x.y/resource-1">>),
  R2 = x_jid:from_bin(<<"a@b.c/resource-1">>),
  ?_assertNot(x_jid:is_same_resource(R1,R2)).

same_resource_is_false_for_different_different_same_domain_resource_test_() ->
  R1 = x_jid:from_bin(<<"a@b.c/resource-1">>),
  R2 = x_jid:from_bin(<<"z@b.c/resource-1">>),
  ?_assertNot(x_jid:is_same_resource(R1,R2)).

has_same_domain_is_true_if_domains_match_test_() ->
  R1 = x_jid:from_bin(<<"a@b.c/resource-1">>),
  R2 = x_jid:from_bin(<<"z@b.c/resource-99">>),
  ?_assert(x_jid:has_same_domain(R1,R2)).

has_same_domain_is_false_if_domains_differ_test_() ->
  R1 = x_jid:from_bin(<<"a@b.c/resource-1">>),
  R2 = x_jid:from_bin(<<"a@x.y/resource-1">>),
  ?_assertNot(x_jid:has_same_domain(R1,R2)).

bare_jid_decomposition_test() ->
  property(new_bare_jid,
           ?FORALL({U,D}, {user(), domain()},
                   begin
                     J = x_jid:new(U, D),

                     false == x_jid:is_full_jid(J) andalso
                     true == x_jid:is_bare_jid(J) andalso
                     U == x_jid:username(J) andalso
                     D == x_jid:domain(J)
                   end)).

full_jid_decomposition_test() ->
  property(new_full_jid,
           ?FORALL({U,D,R}, {user(), domain(), resource()},
                   begin
                     J = x_jid:new(U, D, R),

                     true == x_jid:is_full_jid(J) andalso
                     false == x_jid:is_bare_jid(J) andalso
                     U == x_jid:username(J) andalso
                     D == x_jid:domain(J) andalso
                     R == x_jid:resource(J)
                   end)).

full_jid_to_bare_test() ->
  property(full_to_bare,
           ?FORALL({U,D,R}, {user(), domain(), resource()},
                   begin
                     J = x_jid:new(U, D, R),
                     B = x_jid:bare(J),
                     BB = x_jid:bare(B),

                     false == x_jid:is_bare_jid(J) andalso
                     true == x_jid:is_bare_jid(B) andalso
                     true == x_jid:is_bare_jid(BB) andalso
                     U == x_jid:username(B) andalso
                     D == x_jid:domain(B)
                   end)).


%serialization_test() ->
%    property(serialization_test,
%             ?FORALL({U,D,R}, {user(), domain(), resource()},
%                     begin
%                         J = x_jid:new(U, D, R),
%                         J == x_jid:from_bin(x_jid:to_bin(J))
%                     end)).
%
%% types
user() ->
  ?SUCHTHAT(B, utf8(1023),
            erlang:byte_size(B) < 1024).

domain() ->
  ?SUCHTHAT(B, utf8(1023), erlang:byte_size(B) < 1024 andalso
            erlang:byte_size(B) < 1024).

resource() ->
  ?SUCHTHAT(B, utf8(1023), erlang:byte_size(B) < 1024 andalso
            erlang:byte_size(B) < 1024).

property(Name, Prop) ->
  Props = proper:conjunction([{Name, Prop}]),
  true = proper:quickcheck(Props, [verbose, long_result, {numtests, 100}]).

