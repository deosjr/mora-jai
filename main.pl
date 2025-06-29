:- dynamic(mem/1).

% doesn't find the quickest solution atm
% fail-driven loop finds more but perhaps not all due to mem/1
run :-
	puzzle(orinda, GoalColor, Init),
	solve(GoalColor, Init, Moves),
        draw(Init),
	writeln("---------"),
	draw_solution(Init, Moves).

puzzle(orinda, black, [[green, black,  green],
                       [black, black,  black],
                       [green, yellow, green]]).

solve(Goal, Init, Moves) :-
	assertz(mem(Init)),
	solve(Goal, Init, [], Rev),
	reverse(Rev, Moves).

solve(Goal, State, Ans, Ans) :-
	State = [[Goal, _, Goal], _, [Goal, _, Goal]].

solve(Goal, State, Steps, Ans) :-
	step(Move, State, NewState),
	not(mem(NewState)),
	assertz(mem(NewState)),
	solve(Goal, NewState, [Move|Steps], Ans).

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

print_unicode_emoji(CodePoint) :-
    char_code(Char, CodePoint),
    format("~w~n", [Char]).

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
	writeln(Move),
        draw(Step),
	writeln("---------"),
	draw_solution(Step, Rest).
