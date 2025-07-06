:- dynamic(mem/1).
:- discontiguous step/3.
:- discontiguous puzzle/3.

:- ['blueprints.pl'].

run :-
	puzzle(trading_post, Goal, Init),
	%findall(NewState-Move, step(Move, Init, NewState), Steps),
	%writeln(Steps).
	solve(Goal, Init, Moves),
        draw(Init),
	draw_solution(Init, Moves).

puzzle(trading_post, yellow, [[pink, gray,  gray],
                             [gray,  yellow, yellow],
                             [gray,  yellow, yellow]]).

% inner sanctum puzzles
puzzle(orinda_aries, black, [[green, black,  green],
                             [black, black,  black],
                             [green, yellow, green]]).

puzzle(fenn_aries, red, [[gray,   green, gray],
                         [orange, red,   orange],
                         [white,  green, black]]).

puzzle(arch_aries, yellow, [[black,  yellow, gray],
                            [yellow, green,  yellow],
                            [gray,   yellow, black]]).

puzzle(eraja, violet, [[yellow, violet, yellow],
                       [green,  red,    black],
                       [violet, violet, violet]]).

puzzle(corarica, orange, [[orange, black,  orange],
                          [orange, orange, orange],
                          [violet, green, violet]]).

puzzle(mora-jai, white, [[yellow, yellow, yellow],
                         [white,  pink,   white],
                         [gray,   gray,   gray]]).

puzzle(verra, pink, [[pink,   pink,   gray],
                     [gray,   gray,   gray],
                     [orange, orange, orange]]).

puzzle(nuance, green, [[green, gray,   green],
                       [gray,  orange, orange],
                       [gray,  black,  violet]]).

solve(Goal, Init, Moves) :-
	assertz(mem(Init)),
	solve_(Goal, [Init-[]], Rev),
	reverse(Rev, Moves).

% Goal is either a single color or 4 colors ordered topleft/topright/bottomleft/bottomright
solve_(Goal, [State-Ans| _], Ans) :-
	State = [[Goal, _, Goal], _, [Goal, _, Goal]].

solve_([G1, G2, G3, G4], [State-Ans| _], Ans) :-
	State = [[G1, _, G2], _, [G3, _, G4]].

solve_(Goal, [State-Moves|Fringe], Ans) :-
	findall(NewState-[Move|Moves], new_step(Move, State, NewState), Steps),
	append(Fringe, Steps, NewFringe),
	solve_(Goal, NewFringe, Ans).

new_step(Move, State, NewState) :-
	step(Move, State, NewState),
	not(mem(NewState)),
	assertz(mem(NewState)).

% green (mid does nothing)
step(topleft,     [[green, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[C3, A2, A3], [B1, B2, B3], [C1, C2, green]]).
step(topmid,      [[A1, green, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, C2, A3], [B1, B2, B3], [C1, green, C3]]).
step(topright,    [[A1, A2, green], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, C1], [B1, B2, B3], [green, C2, C3]]).
step(midleft,     [[A1, A2, A3], [green, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B3, B2, green], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, B2, green], [C1, C2, C3]],
                  [[A1, A2, A3], [green, B2, B1], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [green, C2, C3]],
                  [[A1, A2, green], [B1, B2, B3], [A3, C2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, green, C3]],
                  [[A1, green, A3], [B1, B2, B3], [C1, A2, C3]]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, green]],
                  [[green, A2, A3], [B1, B2, B3], [C1, C2, A1]]).

% yellow (top does nothing)
step(midleft,     [[A1, A2, A3], [yellow, B2, B3], [C1, C2, C3]],
                  [[yellow, A2, A3], [A1, B2, B3], [C1, C2, C3]]).
step(middle,      [[A1, A2, A3], [B1, yellow, B3], [C1, C2, C3]],
                  [[A1, yellow, A3], [B1, A2, B3], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, B2, yellow], [C1, C2, C3]],
                  [[A1, A2, yellow], [B1, B2, A3], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [yellow, C2, C3]],
                  [[A1, A2, A3], [yellow, B2, B3], [B1, C2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, yellow, C3]],
                  [[A1, A2, A3], [B1, yellow, B3], [C1, B2, C3]]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, yellow]],
                  [[A1, A2, A3], [B1, B2, yellow], [C1, C2, B3]]).

% black
rotate([A, B, C], [C, A, B]).

step(topleft, [A, B, C], Out) :-
	A = [black, _, _],
	rotate(A, AR),
	Out = [AR, B, C].
step(topmid, [A, B, C], Out) :-
	A = [_, black, _],
	rotate(A, AR),
	Out = [AR, B, C].
step(topright, [A, B, C], Out) :-
	A = [_, _, black],
	rotate(A, AR),
	Out = [AR, B, C].
step(midleft, [A, B, C], Out) :-
	B = [black, _, _],
	rotate(B, BR),
	Out = [A, BR, C].
step(middle, [A, B, C], Out) :-
	B = [_, black, _],
	rotate(B, BR),
	Out = [A, BR, C].
step(midright, [A, B, C], Out) :-
	B = [_, _, black],
	rotate(B, BR),
	Out = [A, BR, C].
step(bottomleft, [A, B, C], Out) :-
	C = [black, _, _],
	rotate(C, CR),
	Out = [A, B, CR].
step(bottommid, [A, B, C], Out) :-
	C = [_, black, _],
	rotate(C, CR),
	Out = [A, B, CR].
step(bottomright, [A, B, C], Out) :-
	C = [_, _, black],
	rotate(C, CR),
	Out = [A, B, CR].

% violet (bottom does nothing)
step(topleft,     [[violet, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[B1, A2, A3], [violet, B2, B3], [C1, C2, C3]]).
step(topmid,      [[A1, violet, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, B2, A3], [B1, violet, B3], [C1, C2, C3]]).
step(topright,    [[A1, A2, violet], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, B3], [B1, B2, violet], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [violet, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [C1, B2, B3], [violet, C2, C3]]).
step(middle,      [[A1, A2, A3], [B1, violet, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, C2, B3], [C1, violet, C3]]).
step(midright,    [[A1, A2, A3], [B1, B2, violet], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, C3], [C1, C2, violet]]).

% orange: depends on neighbours (no diagonals)
majority_color([C, C], C) :- C \= orange.
majority_color([X, Y, Z], C) :-
	C \= orange,
	(X == Y, C = X) ;
	(X == Z, C = X) ;
	(Y == Z, C = Y).
majority_color([W, X, Y, Z], C) :-
	C \= orange,
	(W == X, X == Y, C = W) ;
	(W == X, X == Z, C = W) ;
	(W == Y, Y == Z, C = W) ;
	(X == Y, Y == Z, C = X).

step(topleft,     [[orange, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[Color, A2, A3], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A2, B1], Color).
step(topmid,      [[A1, orange, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, Color, A3], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A1, B2, A3], Color).
step(topright,    [[A1, A2, orange], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, Color], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A2, B3], Color).
step(midleft,     [[A1, A2, A3], [orange, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [Color, B2, B3], [C1, C2, C3]]) :-
	majority_color([A1, B2, C1], Color).
step(middle,      [[A1, A2, A3], [B1, orange, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, Color, B3], [C1, C2, C3]]) :-
	majority_color([A2, B1, B3, C2], Color).
step(midright,    [[A1, A2, A3], [B1, B2, orange], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, Color], [C1, C2, C3]]) :-
	majority_color([A3, B2, C3], Color).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [orange, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [Color, C2, C3]]) :-
	majority_color([B1, C2], Color).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, orange, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, Color, C3]]) :-
	majority_color([C1, B2, C3], Color).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, orange]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, Color]]) :-
	majority_color([B3, C2], Color).

% red
convert_box_red([A, B, C], [X, Y, Z]) :-
	convert_line_red(A, X),
	convert_line_red(B, Y),
	convert_line_red(C, Z).

convert_line_red([A, B, C], [X, Y, Z]) :-
	convert_red(A, X),
	convert_red(B, Y),
	convert_red(C, Z).

convert_red(white, black).
convert_red(black, red).
convert_red(C, C) :- C \= white, C \= black.

step(topleft, Box, Out) :-
	Box = [[red, _, _], _, _],
	convert_box_red(Box, Out).
step(topmid, Box, Out) :-
	Box = [[_, red, _], _, _],
	convert_box_red(Box, Out).
step(topright, Box, Out) :-
	Box = [[_, _, red], _, _],
	convert_box_red(Box, Out).
step(midleft, Box, Out) :-
	Box = [_, [red, _, _], _],
	convert_box_red(Box, Out).
step(middle, Box, Out) :-
	Box = [_, [_, red, _], _],
	convert_box_red(Box, Out).
step(midright, Box, Out) :-
	Box = [_, [_, _, red], _],
	convert_box_red(Box, Out).
step(bottomleft, Box, Out) :-
	Box = [_, _, [red, _, _]],
	convert_box_red(Box, Out).
step(bottommid, Box, Out) :-
	Box = [_, _, [_, red, _]],
	convert_box_red(Box, Out).
step(bottomright, Box, Out) :-
	Box = [_, _, [_, _, red]],
	convert_box_red(Box, Out).

% white: depends on neighbours (no diagonals)
white_convert([], []).
white_convert([white|T], [gray|CT]) :-
	white_convert(T, CT).
white_convert([gray|T], [white|CT]) :-
	white_convert(T, CT).
white_convert([C|T], [C|CT]) :-
	C \= white, C \= gray,
	white_convert(T, CT).

step(topleft,     [[white, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[gray, CA2, A3], [CB1, B2, B3], [C1, C2, C3]]) :-
	white_convert([A2, B1], [CA2, CB1]).
step(topmid,      [[A1, white, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[CA1, gray, CA3], [B1, CB2, B3], [C1, C2, C3]]) :-
	white_convert([A1, B2, A3], [CA1, CB2, CA3]).
step(topright,    [[A1, A2, white], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, CA2, gray], [B1, B2, CB3], [C1, C2, C3]]) :-
	white_convert([A2, B3], [CA2, CB3]).
step(midleft,     [[A1, A2, A3], [white, B2, B3], [C1, C2, C3]],
                  [[CA1, A2, A3], [gray, CB2, B3], [CC1, C2, C3]]) :-
	white_convert([A1, B2, C1], [CA1, CB2, CC1]).
step(middle,      [[A1, A2, A3], [B1, white, B3], [C1, C2, C3]],
                  [[A1, CA2, A3], [CB1, gray, CB3], [C1, CC2, C3]]) :-
	white_convert([A2, B1, B3, C2], [CA2, CB1, CB3, CC2]).
step(midright,    [[A1, A2, A3], [B1, B2, white], [C1, C2, C3]],
                  [[A1, A2, CA3], [B1, CB2, gray], [C1, C2, CC3]]) :-
	white_convert([A3, B2, C3], [CA3, CB2, CC3]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [white, C2, C3]],
                  [[A1, A2, A3], [CB1, B2, B3], [gray, CC2, C3]]) :-
	white_convert([B1, C2], [CB1, CC2]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, white, C3]],
                  [[A1, A2, A3], [B1, CB2, B3], [CC1, gray, CC3]]) :-
	white_convert([C1, B2, C3], [CC1, CB2, CC3]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, white]],
                  [[A1, A2, A3], [B1, B2, CB3], [C1, CC2, gray]]) :-
	white_convert([B3, C2], [CB3, CC2]).

% pink
step(topleft,     [[pink, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[pink, B1, A3], [B2, A2, B3], [C1, C2, C3]]).
step(topmid,      [[A1, pink, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[B1, pink, A1], [B2, B3, A3], [C1, C2, C3]]).
step(topright,    [[A1, A2, pink], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, B2, pink], [B1, B3, A2], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [pink, B2, B3], [C1, C2, C3]],
                  [[C1, A1, A3], [pink, A2, B3], [C2, B2, C3]]).
step(middle,      [[A1, A2, A3], [B1, pink, B3], [C1, C2, C3]],
                  [[B1, A1, A2], [C1, pink, A3], [C2, C3, B3]]).
step(midright,    [[A1, A2, A3], [B1, B2, pink], [C1, C2, C3]],
                  [[A1, B2, A2], [B1, C2, pink], [C1, C3, A3]]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [pink, C2, C3]],
                  [[A1, A2, A3], [C2, B1, B3], [pink, B2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, pink, C3]],
                  [[A1, A2, A3], [C1, B1, B2], [C3, pink, B3]]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, pink]],
                  [[A1, A2, A3], [B1, C2, B2], [C1, B3, pink]]).

% blue: depends on center tile. no effect if center is blue.
% pink/orange can just use their definitions.
% green/yellow/violet/black we need smth special otherwise blue disappears.
% if white in center, blue tiles turn neighbouring gray tiles _blue_
% if red in center, black tiles turn _blue_
step(topleft,     [[blue, A2, A3], [B1, B2, B3], [C1, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
	step(topleft, [[B2, A2, A3], [B1, B2, B3], [C1, C2, C3]], Out).
step(topmid,      [[A1, blue, A3], [B1, B2, B3], [C1, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
        step(topmid, [[A1, B2, A3], [B1, B2, B3], [C1, C2, C3]], Out).
step(topright,    [[A1, A2, blue], [B1, B2, B3], [C1, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
       	step(topright, [[A1, A2, B2], [B1, B2, B3], [C1, C2, C3]], Out).
step(midleft,     [[A1, A2, A3], [blue, B2, B3], [C1, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
       	step(midleft, [[A1, A2, A3], [B2, B2, B3], [C1, C2, C3]], Out).
step(midright,    [[A1, A2, A3], [B1, B2, blue], [C1, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
       	step(midright, [[A1, A2, A3], [B1, B2, B2], [C1, C2, C3]], Out).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [blue, C2, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
        step(bottomleft, [[A1, A2, A3], [B1, B2, B3], [B2, C2, C3]], Out).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, blue, C3]], Out) :-
	( B2 == pink ; B2 == orange ),
        step(bottommid, [[A1, A2, A3], [B1, B2, B3], [C1, B2, C3]], Out).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, blue]], Out) :-
	( B2 == pink ; B2 == orange ),
        step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, B2]], Out).

% duplicates for green/yellow/black/violet/white/red:
step(topleft,     [[blue, A2, A3], [B1, green, B3], [C1, C2, C3]],
                  [[C3, A2, A3], [B1, green, B3], [C1, C2, blue]]).
step(topmid,      [[A1, blue, A3], [B1, green, B3], [C1, C2, C3]],
                  [[A1, C2, A3], [B1, green, B3], [C1, blue, C3]]).
step(topright,    [[A1, A2, blue], [B1, green, B3], [C1, C2, C3]],
                  [[A1, A2, C1], [B1, green, B3], [blue, C2, C3]]).
step(midleft,     [[A1, A2, A3], [blue, green, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B3, green, blue], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, green, blue], [C1, C2, C3]],
                  [[A1, A2, A3], [blue, green, B1], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, green, B3], [blue, C2, C3]],
                  [[A1, A2, blue], [B1, green, B3], [A3, C2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, green, B3], [C1, blue, C3]],
                  [[A1, blue, A3], [B1, green, B3], [C1, A2, C3]]).
step(bottomright, [[A1, A2, A3], [B1, green, B3], [C1, C2, blue]],
                  [[blue, A2, A3], [B1, green, B3], [C1, C2, A1]]).

step(midleft,     [[A1, A2, A3], [blue, yellow, B3], [C1, C2, C3]],
                  [[blue, A2, A3], [A1, yellow, B3], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, yellow, blue], [C1, C2, C3]],
                  [[A1, A2, blue], [B1, yellow, A3], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, yellow, B3], [blue, C2, C3]],
                  [[A1, A2, A3], [blue, yellow, B3], [B1, C2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, yellow, B3], [C1, blue, C3]],
                  [[A1, A2, A3], [B1, blue, B3], [C1, yellow, C3]]).
step(bottomright, [[A1, A2, A3], [B1, yellow, B3], [C1, C2, blue]],
                  [[A1, A2, A3], [B1, yellow, blue], [C1, C2, B3]]).

step(topleft,     [[blue, A2, A3], [B1, black, B3], [C1, C2, C3]],
                  [[A3, blue, A2], [B1, black, B3], [C1, C2, C3]]).
step(topmid,      [[A1, blue, A3], [B1, black, B3], [C1, C2, C3]],
                  [[A3, A1, blue], [B1, black, B3], [C1, C2, C3]]).
step(topright,    [[A1, A2, blue], [B1, black, B3], [C1, C2, C3]],
                  [[blue, A1, A2], [B1, black, B3], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [blue, black, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B3, blue, black], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, black, blue], [C1, C2, C3]],
                  [[A1, A2, A3], [blue, B1, black], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, black, B3], [blue, C2, C3]],
                  [[A1, A2, A3], [B1, black, B3], [C3, blue, C2]]).
step(bottommid,   [[A1, A2, A3], [B1, black, B3], [C1, blue, C3]],
                  [[A1, A2, A3], [B1, black, B3], [C3, C1, blue]]).
step(bottomright, [[A1, A2, A3], [B1, black, B3], [C1, C2, blue]],
                  [[A1, A2, A3], [B1, black, B3], [blue, C1, C2]]).

step(topleft,     [[blue, A2, A3], [B1, violet, B3], [C1, C2, C3]],
                  [[B1, A2, A3], [blue, violet, B3], [C1, C2, C3]]).
step(topmid,      [[A1, blue, A3], [B1, violet, B3], [C1, C2, C3]],
                  [[A1, violet, A3], [B1, blue, B3], [C1, C2, C3]]).
step(topright,    [[A1, A2, blue], [B1, violet, B3], [C1, C2, C3]],
                  [[A1, A2, B3], [B1, violet, blue], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [blue, violet, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [C1, violet, B3], [blue, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, violet, blue], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, violet, C3], [C1, C2, blue]]).

blue_convert([], []).
blue_convert([blue|T], [gray|CT]) :-
	blue_convert(T, CT).
blue_convert([gray|T], [blue|CT]) :-
	blue_convert(T, CT).
blue_convert([C|T], [C|CT]) :-
	C \= blue, C \= gray,
	blue_convert(T, CT).

step(topleft,     [[blue, A2, A3], [B1, white, B3], [C1, C2, C3]],
                  [[gray, CA2, A3], [CB1, white, B3], [C1, C2, C3]]) :-
	blue_convert([A2, B1], [CA2, CB1]).
step(topmid,      [[A1, blue, A3], [B1, white, B3], [C1, C2, C3]],
                  [[CA1, gray, CA3], [B1, white, B3], [C1, C2, C3]]) :-
	blue_convert([A1, A3], [CA1, CA3]).
step(topright,    [[A1, A2, blue], [B1, white, B3], [C1, C2, C3]],
                  [[A1, CA2, gray], [B1, white, CB3], [C1, C2, C3]]) :-
	blue_convert([A2, B3], [CA2, CB3]).
step(midleft,     [[A1, A2, A3], [blue, white, B3], [C1, C2, C3]],
                  [[CA1, A2, A3], [gray, white, B3], [CC1, C2, C3]]) :-
	blue_convert([A1, C1], [CA1, CC1]).
step(midright,    [[A1, A2, A3], [B1, white, blue], [C1, C2, C3]],
                  [[A1, A2, CA3], [B1, white, gray], [C1, C2, CC3]]) :-
	blue_convert([A3, C3], [CA3, CC3]).
step(bottomleft,  [[A1, A2, A3], [B1, white, B3], [blue, C2, C3]],
                  [[A1, A2, A3], [CB1, white, B3], [gray, CC2, C3]]) :-
	blue_convert([B1, C2], [CB1, CC2]).
step(bottommid,   [[A1, A2, A3], [B1, white, B3], [C1, blue, C3]],
                  [[A1, A2, A3], [B1, white, B3], [CC1, gray, CC3]]) :-
	blue_convert([C1, C3], [CC1, CC3]).
step(bottomright, [[A1, A2, A3], [B1, white, B3], [C1, C2, blue]],
                  [[A1, A2, A3], [B1, white, CB3], [C1, CC2, gray]]) :-
	blue_convert([B3, C2], [CB3, CC2]).

convert_box_blue([A, B, C], [X, Y, Z]) :-
	convert_line_blue(A, X),
	convert_line_blue(B, Y),
	convert_line_blue(C, Z).

convert_line_blue([A, B, C], [X, Y, Z]) :-
	convert_blue(A, X),
	convert_blue(B, Y),
	convert_blue(C, Z).

convert_blue(white, black).
convert_blue(black, blue).
convert_blue(C, C) :- C \= white, C \= black.

step(topleft, Box, Out) :-
	Box = [[blue, _, _], [_, red, _], _],
	convert_box_blue(Box, Out).
step(topmid, Box, Out) :-
	Box = [[_, blue, _], [_, red, _], _],
	convert_box_blue(Box, Out).
step(topright, Box, Out) :-
	Box = [[_, _, blue], [_, red, _], _],
	convert_box_blue(Box, Out).
step(midleft, Box, Out) :-
	Box = [_, [blue, red, _], _],
	convert_box_blue(Box, Out).
step(midright, Box, Out) :-
	Box = [_, [_, red, blue], _],
	convert_box_blue(Box, Out).
step(bottomleft, Box, Out) :-
	Box = [_, [_, red, _], [blue, _, _]],
	convert_box_blue(Box, Out).
step(bottommid, Box, Out) :-
	Box = [_, [_, red, _], [_, blue, _]],
	convert_box_blue(Box, Out).
step(bottomright, Box, Out) :-
	Box = [_, [_, red, _], [_, _, blue]],
	convert_box_blue(Box, Out).

emoji(green, 0x1F7E9).
emoji(yellow, 0x1F7E8).
emoji(black, 0x2B1B).
emoji(orange, 0x1F7E7).
emoji(red, 0x1F7E5).
emoji(violet, 0x1F7EA).
emoji(white, 0x2B1C).
emoji(blue, 0x1F7E6).
% use hearts for unsupported colors
emoji(gray, 0x1FA76).
emoji(pink, 0x1FA77).
emoji(test, 0x274C).

print_unicode_emoji(CodePoint) :-
    char_code(Char, CodePoint),
    format("~w", [Char]).

draw([A, B, C]) :-
	draw_line(A),
	draw_line(B),
	draw_line(C).

draw_line([X, Y, Z]) :-
	emoji(X, XE),
	emoji(Y, YE),
	emoji(Z, ZE),
    	char_code(CX, XE),
    	char_code(CY, YE),
    	char_code(CZ, ZE),
    	format("~w~w~w~n", [CX, CY, CZ]).

draw_solution(_, []).
draw_solution(State, [Move|Rest]) :-
        step(Move, State, Step),
	writeln("---------"),
	writeln(Move),
        draw(Step),
	draw_solution(Step, Rest).
