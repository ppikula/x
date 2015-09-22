%% vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
%% -*- mode: erlang; erlang-indent-level: 2 -*- %%
-module(x_stream_features).

-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

%% convert to exml
-behaviour(x_exmlable).
-export([to_exmlel/1]).

-export([new/0, new/1,
         add/3, add/4,
         has_feature/3
        ]).

-export_type([x_stream_features/0]).

-record(simple_feature, {
          name :: binary(),
          ns :: x_ns:x_ns() }).

-record(complex_feature, {
          name :: binary(),
          ns :: x_ns:x_ns(),
          children :: [xmlstreamelement()]}).

-record(x_stream_features, {features = [] :: [feature()]}).



-type complex_feature() :: #complex_feature{}.
-type simple_feature() :: #simple_feature{}.
-type feature() :: simple_feature() | complex_feature().
-type x_stream_features() :: #x_stream_features{}.


new() ->
  #x_stream_features{}.

new(#xmlel{name = <<"stream:features">>, children = C}) ->
  Features = lists:map(fun feature_from_exml/1, C),
  #x_stream_features{ features = Features}.

-spec add(x_stream_features(), binary(), x_ns:x_ns()) -> x_stream_features().
add(Features, FeatureName, FeatureNamespace) ->
  add(Features, FeatureName, FeatureNamespace, []).

-spec add(x_stream_features(), binary(), x_ns:x_ns(), [xmlstreamelement()]) ->
  x_stream_features().
add(Features, FeatureName, FeatureNamespace, Subelements) ->
  case has_feature(Features, FeatureNamespace, FeatureName) of
    false ->
      #x_stream_features{features = CurrentFeatures} = Features,
      NewFeature = feature(FeatureName, FeatureNamespace, Subelements),
      NewFeaturesList = CurrentFeatures ++ [NewFeature],
      Features#x_stream_features{features = NewFeaturesList};
    true ->
      Features
  end.

-spec has_feature(x_stream_features(), x_ns:x_ns(), binary()) -> boolean().
has_feature(Features, Namespace, Name) ->
  #x_stream_features{features = CurrentFeatures} = Features,
  F = fun(#simple_feature{ns = NS, name = N}) ->
          N == Name andalso x_ns:equals(NS, Namespace);
         (#complex_feature{ns = NS, name = N}) ->
          N == Name andalso x_ns:equals(NS, Namespace)
      end,
  1 == erlang:length(lists:filter(F, CurrentFeatures)).

-spec to_exmlel(tuple()) -> #xmlel{}.
to_exmlel(#x_stream_features{features = Features}) ->
  EXMLFeatures = lists:map(fun to_exmlel/1, Features),
  #xmlel{name = <<"stream:features">>, children = EXMLFeatures};
to_exmlel(#simple_feature{name = N, ns = Namespace}) ->
  #xmlel{name = N, attrs = [x_ns:to_attr(Namespace)]};
to_exmlel(#complex_feature{name = N, ns = Namespace, children = C}) ->
  #xmlel{name = N, attrs = [x_ns:to_attr(Namespace)], children = C}.

-spec feature(binary(), x_ns:x_ns()) -> simple_feature().
feature(Name, NS) ->
  #simple_feature{name = Name, ns = NS}.

-spec feature(binary(), x_ns:x_ns(), [xmlstreamelement()]) -> feature().
feature(Name, NS, []) ->
  feature(Name, NS);
feature(Name, NS, SubEl) ->
  #complex_feature{name = Name, ns = NS, children = SubEl}.

-spec feature_from_exml(xmlstreamelement()) -> feature().
feature_from_exml(#xmlel{name = Name, children = C} = El) ->
  NS = x_ns:new(exml_query:attr(El, <<"xmlns">>, <<>>)),
  feature(Name, NS, C).
