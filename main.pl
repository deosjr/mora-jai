:- dynamic(mem/1).
:- discontiguous step/3.

run :-
	puzzle(corarica, GoalColor, Init),
	%findall(NewState-Move, step(Move, Init, NewState), Steps),
	%writeln(Steps),
	solve(GoalColor, Init, Moves),
        draw(Init),
	draw_solution(Init, Moves).

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

solve(Goal, Init, Moves) :-
	assertz(mem(Init)),
	solve_(Goal, [Init-[]], Rev),
	reverse(Rev, Moves).

solve_(Goal, [State-Ans| _], Ans) :-
	State = [[Goal, _, Goal], _, [Goal, _, Goal]].

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
step(topleft,     [[black, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A3, black, A2], [B1, B2, B3], [C1, C2, C3]]).
step(topmid,      [[A1, black, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A3, A1, black], [B1, B2, B3], [C1, C2, C3]]).
step(topright,    [[A1, A2, black], [B1, B2, B3], [C1, C2, C3]],
                  [[black, A1, A2], [B1, B2, B3], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [black, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B3, black, B2], [C1, C2, C3]]).
step(middle,      [[A1, A2, A3], [B1, black, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B3, B1, black], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, B2, black], [C1, C2, C3]],
                  [[A1, A2, A3], [black, B1, B2], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [black, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C3, black, C2]]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, black, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C3, C1, black]]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, black]],
                  [[A1, A2, A3], [B1, B2, B3], [black, C1, C2]]).

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
majority_color([C, C], C).
majority_color([X, Y, Z], C) :-
	(X == Y, C = X) ;
	(X == Z, C = X) ;
	(Y == Z, C = Y).
majority_color([W, X, Y, Z], C) :-
	(W == X, X == Y, C = W) ;
	(W == X, X == Z, C = W) ;
	(W == Y, Y == Z, C = W) ;
	(X == Y, Y == Z, C = X).

step(topleft,     [[orange, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[Color, A2, A3], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A2, B1], Color), Color \= orange.
step(topmid,      [[A1, orange, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, Color, A3], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A1, B2, A3], Color), Color \= orange.
step(topright,    [[A1, A2, orange], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, Color], [B1, B2, B3], [C1, C2, C3]]) :-
	majority_color([A2, B3], Color), Color \= orange.
step(midleft,     [[A1, A2, A3], [orange, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [Color, B2, B3], [C1, C2, C3]]) :-
	majority_color([A1, B2, C1], Color), Color \= orange.
step(middle,      [[A1, A2, A3], [B1, orange, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, Color, B3], [C1, C2, C3]]) :-
	majority_color([A2, B1, B3, C2], Color), Color \= orange.
step(midright,    [[A1, A2, A3], [B1, B2, orange], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, Color], [C1, C2, C3]]) :-
	majority_color([A3, B2, C3], Color), Color \= orange.
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [orange, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [Color, C2, C3]]) :-
	majority_color([B1, C2], Color), Color \= orange.
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, orange, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, Color, C3]]) :-
	majority_color([C1, B2, C3], Color), Color \= orange.
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, orange]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, Color]]) :-
	majority_color([B3, C2], Color), Color \= orange.
/*
step(topleft,     [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(topmid,      [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(topright,    [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(midleft,     [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(middle,      [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(midright,    [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(bottomleft,  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(bottommid,   [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
step(bottomright, [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
                  [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]).
*/

emoji(green, 0x1F7E9).
emoji(yellow, 0x1F7E8).
emoji(black, 0x2B1B).
emoji(orange, 0x1F7E7).
emoji(red, 0x1F7E5).
emoji(violet, 0x1F7EA).
emoji(white, 0x2B1C).
% use hearts for unsupported colors
emoji(gray, 0x1FA76).
emoji(pink, 0x1FA77).

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
