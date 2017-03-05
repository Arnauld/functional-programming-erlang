-module(nub).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([nub/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
nub(XS) ->
  nub(XS, []).

nub([], Acc) -> reverse(Acc);
nub([X | XS], Acc) ->
  case contains(Acc, X) of
    true ->
      nub(XS, Acc);
    _ ->
      nub(XS, [X | Acc])
  end.

reverse(XS) ->
  reverse(XS, []).
reverse([], Acc) -> Acc;
reverse([X | XS], Acc) -> reverse(XS, [X | Acc]).

contains([], _) -> false;
contains([X | _XS], X) -> true;
contains([_ | XS], X) -> contains(XS, X).

% -------------------------------------
% TEST
% -------------------------------------
%
% eunit:test(shabits).
%
-include_lib("eunit/include/eunit.hrl").

nub_test() ->
  ?assertEqual([], nub([])),
  ?assertEqual([3], nub([3])),
  ?assertEqual([1, 3], nub([1, 3])),
  ?assertEqual([2, 4, 1, 3], nub([2, 4, 1, 3, 3, 1])).




