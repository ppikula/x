-module(x_namespacable).

-export([get_ns/1]).

-callback get_ns(any()) -> x_ns:x_ns().

-spec get_ns(tuple()) -> x_ns:x_ns().
get_ns(Namespacable) ->
  (x_implementation:module(Namespacable)):get_ns().

