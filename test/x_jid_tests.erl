-module(x_jid_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%%  Simple tests
%%


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

