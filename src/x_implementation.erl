-module(x_implementation).

-export([module/1]). 

-spec module(tuple()) -> module().
module(Record) when is_tuple(Record) ->
  element(1, Record).
