-module(sixth).
-export([perimeter/1, area/1, enclose/1, bits/1]).

% Representation: {triangle, {X, Y}, B, H}
% {X, Y} is centre, B is Base length and H is Height of the triangle.
% Assuming the triangle is a right angled triangle because:
% With lengths of all sides it is difficult to find height
% which is required for enclose.
% With base and height, perimeter is impossible to calculate.
% Hence assuming right angled triangle with base and height.

perimeter({circle, {_X, _Y}, R}) ->
  2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) ->
  2 * (H + W);
perimeter({triangle, {_X, _Y}, B, H}) ->
  HYP = math:sqrt(B*B + H*H),
  B + H + HYP.

area({circle, {_X, _Y}, R}) ->
  math:pi() * R * R;
area({rectangle, {_X, _Y}, H, W}) ->
  H * W;
area({triangle, {_X, _Y}, B, H}) ->
  B * H / 2.

enclose({circle, {X, Y}, R}) ->
  {rectangle, {X, Y}, 2*R, 2*R};    % square with centre same as circle
enclose({rectangle, {X, Y}, W, H}) ->
  {rectangle, {X, Y}, W, H};        % return back the same rectangle
enclose({triangle, {X, Y}, B, H}) ->
  {rectangle, {X, Y}, B, H}.        % rectangle with base and height of triangle


% Sum of bits in binary representation of number
bits(N) ->
  bits(N, 0).

% Keep on dividing by zero and take remainder at each step
% Increment the count when remainder is 1
bits(1, ACC) ->
  ACC + 1;
bits(N, ACC) when N rem 2 == 1 ->
  bits(N div 2, ACC + 1);
bits(N, ACC) ->
  bits(N div 2, ACC).


% -------------------------------------
% TEST
% -------------------------------------
%
% eunit:test(shabits).
%
-include_lib("eunit/include/eunit.hrl").

bits_test() ->
  ?assertEqual(0, bits(2#0)),
  ?assertEqual(1, bits(2#1)),
  ?assertEqual(3, bits(2#111)),
  ?assertEqual(4, bits(2#100011100)).