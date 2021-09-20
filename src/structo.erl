%% @doc structo
%% extract, update or remove values in JSON messages

-module(structo).
-export([get/2,set/3,del/2,make/2]).

% Examples:
%
% 1. Extract Action.
%
% Msg = {struct,[{<<"action">>,<<"update">>},
%         {<<"id">>,74},
%         {<<"doc">>,
%          {struct,[{<<"text">>,<<>>},
%                   {<<"x">>,96},
%                   {<<"y">>,58},
%                   {<<"z">>,3800},
%                   {<<"color">>,<<"yellow">>}]}}]}.
%
% Action = structo:get(<<"action">>, Msg).
% Action.
%      <<"update">>
%
% 2. Update action to <<"insert">>:
%
% structo:set(<<"action">>, <<"insert">>,  Msg).
% Msg.
%
%    {struct,[{<<"action">>,<<"insert">>},
%    ... skipped ...
%                   {<<"color">>,<<"yellow">>}]}}]}.
%

get(K,{struct, S}) ->
    proplists:get_value(K,S).

set(K,V,{struct,S}) ->
    case get(K,{struct,S}) of
        undefined -> {struct, [{K,V}] ++ S};
        _ -> {struct, lists:keyreplace(K,1,S,{K,V})}
    end.

del(K,{struct, S}) ->
    {struct, proplists:delete(K,S)}.

make(K,V) -> {struct,[{K,V}]}.
