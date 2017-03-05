-module(shabits).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([perimeter/1, area/1, enclose/1]).
-export([bits/1, bits_tailrec/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
perimeter({triangle, A = {_Xa, _Ya}, B = {_Xb, _Yb}, C = {_Xc, _Yc}}) ->
  distance(A, B) + distance(B, C) + distance(C, A);

perimeter({rectangle, A = {Xmin, Ymin}, _C = {Xmax, Ymax}}) ->
  % ^
  % |
  % x...D-----------C
  % |   |           |
  % x...A-----------B
  % |   :           :
  % +---x-----------x--------->
  %
  B = {Xmax, Ymin},
  D = {Xmin, Ymax},
  2 * (distance(A, D) + distance(A, B)).

area({triangle, A = {_Xa, _Ya}, B = {_Xb, _Yb}, C = {_Xc, _Yc}}) ->
  % Heron's formula
  DA = distance(A, B),
  DB = distance(B, C),
  DC = distance(C, A),
  S = (DA + DB + DC) / 2,
  math:sqrt(S * (S - DA) * (S - DB) * (S - DC));
area({rectangle, A = {Xmin, Ymin}, {Xmax, Ymax}}) ->
  B = {Xmax, Ymin},
  D = {Xmin, Ymax},
  distance(A, D) * distance(A, B).

enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  {Xmin, Xmax} = min_max([X1, X2, X3], X1, X1),
  {Ymin, Ymax} = min_max([Y1, Y2, Y3], Y1, Y1),
  {rectangle, {Xmin, Ymin}, {Xmax, Ymax}};
enclose(Rectangle = {rectangle, _A, _C}) ->
  Rectangle.

distance({X1, Y1}, {X2, Y2}) ->
  DX = (X2 - X1),
  DY = (Y2 - Y1),
  math:sqrt(DX * DX + DY * DY).

min_max([], Min, Max) -> {Min, Max};
min_max([X | XS], Min, Max) when X < Min -> min_max(XS, X, Max);
min_max([X | XS], Min, Max) when X > Max -> min_max(XS, Min, X);
min_max([_ | XS], Min, Max) -> min_max(XS, Min, Max).


bits(N) when is_integer(N), N >= 0 ->
  bits0(N).

bits0(0) -> 0;
bits0(N) -> (N band 2#1) + bits0(N bsr 1).

bits_tailrec(N) when is_integer(N), N >= 0 ->
  bits_tailrec(N, 0).

bits_tailrec(0, Acc) -> Acc;
bits_tailrec(N, Acc) -> bits_tailrec(N bsr 1, (N band 2#1) + Acc).


% -------------------------------------
% TEST
% -------------------------------------
%
% eunit:test(shabits).
%
-include_lib("eunit/include/eunit.hrl").

distance_test() ->
  ?assertEqual(4.0, distance({0, 0}, {4, 0})),
  ?assertEqual(math:sqrt(2), distance({0, 0}, {1, 1})).

min_max_test() ->
  ?assertEqual({0, 0}, min_max([], 0, 0)),
  ?assertEqual({1, 2}, min_max([1], 2, 2)),
  ?assertEqual({2, 12}, min_max([4, 5, 6, 2, 11, 7, 12, 11], 4, 4)).

perimeter_test() ->
  ?assertEqual(1.0 * (3 + 3 + 4 + 4), perimeter({rectangle, {0, 0}, {3, 4}})),
  ?assertEqual(1.0 * (3 + 4 + 5), perimeter({triangle, {0, 0}, {3, 0}, {0, 4}})).

area_test() ->
  ?assertEqual(1.0 * (3 * 4), area({rectangle, {0, 0}, {3, 4}})),
  ?assertEqual(0.5 * (3 * 4), area({triangle, {0, 0}, {3, 0}, {0, 4}})).

enclose_test() ->
  ?assertEqual({rectangle, {0, 0}, {3, 4}}, enclose({rectangle, {0, 0}, {3, 4}})),
  ?assertEqual({rectangle, {0, 0}, {3, 4}}, enclose({triangle, {0, 0}, {3, 0}, {0, 4}})).

bits_test() ->
  ?assertEqual(0, bits(2#0)),
  ?assertEqual(1, bits(2#1)),
  ?assertEqual(3, bits(2#111)),
  ?assertEqual(4, bits(2#100011100)).

bits_tailrec_test() ->
  ?assertEqual(0, bits_tailrec(2#0)),
  ?assertEqual(1, bits_tailrec(2#1)),
  ?assertEqual(3, bits_tailrec(2#111)),
  ?assertEqual(4, bits_tailrec(2#100011100)).