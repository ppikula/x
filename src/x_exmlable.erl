-module(x_exmlable).

-include_lib("exml/include/exml_stream.hrl").

-export([to_exmlel/1]).

%% xmlstreamelement - both xmlel and streams start/stop
-callback to_exmlel(any()) -> exml:element().

-spec to_exmlel(tuple()) -> exml:element().
to_exmlel(Xmlable) ->
    (x_implementation:module(Xmlable)):to_exmlel(Xmlable).
