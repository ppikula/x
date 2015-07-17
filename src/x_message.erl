-module(x_message).
-export([new/3, is_message/1]).

-behaviour(x_Addressable).
-export([from/1, to/1]).

-export_type([x_message/0]).
-record(x_message, {from :: x_jid:jid(),
                    to :: x_jid:jid()}).
-type x_message() :: #x_message{}.

-spec new(x_jid:jid(), x_jid:jid(), iolist()) -> x_message().
new(From, To, _Body) ->
    x_contract:check(From, fun x_jid:is_jid/1),
    x_contract:check(To, fun x_jid:is_jid/1),
    #x_message{from = From, to = To}.

-spec is_message(any()) -> boolean().
is_message(#x_message{}) -> true;
is_message(_) -> false.

-spec from(x_message()) -> x_jid:jid().
from(#x_message{from = From}) -> From.

-spec to(x_message()) -> x_jid:jid().
to(#x_message{to = To}) -> To.
