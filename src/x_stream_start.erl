%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_stream_start).
-include_lib("exml/include/exml_stream.hrl").

%% API
-export([new/3]).

%% validators
-export([is_stream_start/1]).

%% convert to exml
-behaviour(x_exmlable).
-export([to_exmlel/1]).

%% main type
-record(x_stream_start,  {to :: binary(),
                          lang :: x:iso_639_1_lang(),
                          version :: float()}).
-type x_stream_start() :: #x_stream_start{}.

-spec new(x_jid:jid(), x:iso_639_1_lang(), float()) -> x_stream_start().
new(Domain, Lang, Version) ->
  x_contract:check(Domain, fun x_jid:is_jid/1),
  %% TODO: check version and lang ?
  #x_stream_start{to = x_jid:to_bin(Domain),
                  lang = Lang,
                  version = Version}.

-spec is_stream_start(any()) -> boolean().
is_stream_start(#x_stream_start{}) -> true;
is_stream_start(_) -> false.

-spec to_exmlel(x_stream_start()) -> #xmlstreamstart{}.
to_exmlel(StreamStart) ->
  #x_stream_start{to = Domain, lang = Lang, version = Version} = StreamStart,
  Attrs = [{<<"to">>, Domain},
           {<<"xml:lang">>, erlang:atom_to_binary(Lang, utf8)},
           {<<"version">>, float_to_binary(Version, [{decimals, 1}])}],
  #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.
