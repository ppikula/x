%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
-module(x_ns).

-export([new/1,
         to_attr/1,
         is_ns/1,
         equals/2,
         is_subnamespace_of/2]).

-include_lib("exml/include/exml.hrl").

-record(x_ns, { uri = <<>> :: binary() }).

-export_type([x_ns/0]).

-type x_ns() :: #x_ns{}.

-spec new(binary()) -> x_ns().
new(Namespace) ->
  x_contract:check(Namespace, fun is_binary/1),
  #x_ns{uri = Namespace}.

-spec to_attr(x_ns()) -> {<<_:40>>,  binary()}.
to_attr(#x_ns{uri = U}) ->
  {<<"xmlns">>, U}.

-spec equals(x_ns(), x_ns()) -> boolean().
equals(#x_ns{} = A, #x_ns{} = A) -> true;
equals(#x_ns{}, #x_ns{}) -> false.

-spec is_subnamespace_of(x_ns(), x_ns()) -> boolean().
is_subnamespace_of(Namespace, BaseNamespace) ->
  x_contract:check(Namespace, fun is_ns/1),
  x_contract:check(BaseNamespace, fun is_ns/1),

  N = erlang:byte_size(BaseNamespace#x_ns.uri),
  Binaries = [Namespace#x_ns.uri, BaseNamespace#x_ns.uri],
  case binary:longest_common_prefix(Binaries)  of
    N -> true;
    _ -> false
  end.

-spec is_ns(any()) -> boolean().
is_ns(#x_ns{}) -> true;
is_ns(_) -> false.
