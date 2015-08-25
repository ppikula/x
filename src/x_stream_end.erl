-module(x_stream_end).
-include_lib("exml/include/exml_stream.hrl").

%% API
-export([new/0]).

%% validators
-export([is_stream_end/1]).

%% convert to exml
-behaviour(x_Xmlable).
-export([to_exmlel/1]).

%% main type
-record(x_stream_end,  {}).
-type x_stream_end() :: #x_stream_end{}.

-spec new() -> x_stream_end().
new() ->
    #x_stream_end{}.

-spec is_stream_end(any()) -> boolean().
is_stream_end(#x_stream_end{}) -> true;
is_stream_end(_) -> false.

to_exmlel(#x_stream_end{}) ->
    #xmlstreamend{name = <<"stream:stream">>}.

