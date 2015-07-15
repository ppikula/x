-module(x_jid).

-export([new/2, new/3]).
-export([to_bin/1]).
-export([from_bin/1]).
-export([username/1]).
-export([domain/1]).
-export([resource/1]).
-export([bare/1]).
-export([is_bare_jid/1]).
-export([is_full_jid/1]).
-export([is_same_user/2]).
-export([is_same_resource/2]).
-export([has_same_domain/2]).

-export_type([jid/0,
              full_jid/0,
              bare_jid/0,
              username/0,
              domain/0,
              resource/0]).

-record(full_jid, {
          username :: username(),
          domain :: domain(),
          resource :: resource()
         }).

-record(bare_jid, {
          username :: username(),
          domain :: domain()
         }).

-type jid() :: full_jid() | bare_jid().

-type full_jid() :: #full_jid{}.
-type bare_jid() :: #bare_jid{}.

-type username() :: binary().
-type domain() :: binary().
-type resource() :: binary().

-spec new(username(), domain(), resource()) -> full_jid().
new(User, Domain, Resource) when is_binary(User), is_binary(Domain), is_binary(Resource) ->
    %% FIXME check constraints
    #full_jid{username = User, domain = Domain, resource = Resource}.

-spec new(username(), domain()) -> bare_jid().
new(User, Domain) when is_binary(User), is_binary(Domain) ->
    %% FIXME check constraints
    #bare_jid{username = User, domain = Domain}.

-spec to_bin(jid()) -> binary().
to_bin(#full_jid{username = U, domain = D, resource = R}) ->
    <<(maybe_user_at(U))/binary,D/binary,"/",R/binary>>;
to_bin(#bare_jid{username = U, domain = D}) ->
    <<(maybe_user_at(U))/binary,D/binary>>.

-spec from_bin(binary()) -> jid().
from_bin(Bin) when is_binary(Bin) ->
    case parse_jid_elements(Bin) of
        {U, D} -> #bare_jid{username = U, domain = D};
        {U, D, R} -> #full_jid{username = U, domain = D, resource = R}
    end.

-spec username(jid()) -> username().
username(#full_jid{username = U}) -> U;
username(#bare_jid{username = U}) -> U.

-spec domain(jid()) -> domain().
domain(#full_jid{domain = D}) -> D;
domain(#bare_jid{domain = D}) -> D.

-spec resource(full_jid()) -> resource().
resource(#full_jid{resource = R}) -> R.

-spec bare(jid()) -> bare_jid().
bare(#full_jid{username = U, domain = D}) -> new(U,D);
bare(#bare_jid{} = JID) -> JID.

-spec is_bare_jid(jid()) -> boolean().
is_bare_jid(#bare_jid{}) -> true;
is_bare_jid(#full_jid{}) -> false.

-spec is_full_jid(jid()) -> boolean().
is_full_jid(#bare_jid{}) -> false;
is_full_jid(#full_jid{}) -> true.

-spec is_same_user(jid(), jid()) -> boolean().
is_same_user(JID1, JID2) ->
    bare(JID1) == bare(JID2).

-spec is_same_resource(full_jid(), full_jid()) -> boolean().
is_same_resource(#full_jid{} = JID, #full_jid{} = JID) -> true;
is_same_resource(#full_jid{} = _JID1, #full_jid{} = _JID2) -> false.

-spec has_same_domain(jid(), jid()) -> boolean().
has_same_domain(JID1, JID2) ->
    domain(JID1) == domain(JID2).

-spec parse_jid_elements(binary()) -> {username(), domain()} | {username(), domain(), resource()} | error.
parse_jid_elements(Bin) ->
    binary_to_jid(Bin).

binary_to_jid(J) ->
    binary_to_jid1(J, <<>>).

%% TODO: fix this shit
binary_to_jid1(<<$@, _J/binary>>, <<>>) ->
    error;
binary_to_jid1(<<$@, J/binary>>, N) ->
    binary_to_jid2(J, binary_reverse(N), <<>>);
binary_to_jid1(<<$/, _J/binary>>, <<>>) ->
    error;
binary_to_jid1(<<$/, J/binary>>, N) ->
    binary_to_jid3(J, <<>>, binary_reverse(N), <<>>);
binary_to_jid1(<<C, J/binary>>, N) ->
    binary_to_jid1(J, <<C, N/binary>>);
binary_to_jid1(<<>>, <<>>) ->
    error;
binary_to_jid1(<<>>, N) ->
    {<<>>, binary_reverse(N)}.


%% @doc Only one "@" is admitted per JID ?
binary_to_jid2(<<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_jid2(<<$/, _J/binary>>, _N, <<>>) ->
    error;
binary_to_jid2(<<$/, J/binary>>, N, S) ->
    binary_to_jid3(J, N, binary_reverse(S), <<>>);
binary_to_jid2(<<C, J/binary>>, N, S) ->
    binary_to_jid2(J, N, <<C, S/binary>>);
binary_to_jid2(<<>>, _N, <<>>) ->
    error;
binary_to_jid2(<<>>, N, S) ->
    {N, binary_reverse(S)}.


binary_to_jid3(<<C, J/binary>>, N, S, R) ->
    binary_to_jid3(J, N, S, <<C, R/binary>>);
binary_to_jid3(<<>>, N, S, R) ->
    {N, S, binary_reverse(R)}.


-spec binary_reverse(binary()) -> binary().
binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H,T/binary>>) ->
    <<(binary_reverse(T))/binary,H>>.

maybe_user_at(<<>>) -> <<>>;
maybe_user_at(User) -> <<User/binary, "@">>.

