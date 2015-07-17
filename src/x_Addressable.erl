-module(x_Addressable).

-export([from/1, to/1]).

-callback from(any()) -> x_jid:jid().
-callback to(any()) -> x_jid:jid().

-spec from(tuple()) -> x_jid:jid().
from(Addressable) ->
  (x_implementation:module(Addressable)):from(Addressable).

-spec to(tuple()) -> x_jid:jid().
to(Addressable) ->
  (x_implementation:module(Addressable)):to(Addressable).
