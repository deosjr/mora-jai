% WARNING: SPOILERS FOR BLUE PRINCE !

core(A, B, C, D, Core) :-
    Core is (A / B - C) * D.
core(A, B, C, D, Core) :-
    Core is A / B * C - D.
core(A, B, C, D, Core) :-
    % HAND -> 2, since 8 - 1 / 14 * 4 = 2, but is a float..
    %Core is (A - B) / C * D.
    Core is (A - B) * D / C.
core(A, B, C, D, Core) :-
    Core is (A * B) / C - D.
core(A, B, C, D, Core) :-
    Core is (A - B) * C / D.
core(A, B, C, D, Core) :-
    Core is (A * B - C) / D.

find_core(A, B, C, D, Core) :-
    findall(X, (core(A, B, C, D, X), integer(X), X > 0), Candidates),
    sort(Candidates, [Core|_]).

string_to_nums(Str, A, B, C, D) :-
    string_codes(Str, [AX, BX, CX, DX]),
    A is AX - 64,
    B is BX - 64,
    C is CX - 64,
    D is DX - 64.

string_to_core(Str, Core) :-
    string_to_nums(Str, A, B, C, D),
    find_core(A, B, C, D, Core).

alphabet_to_ascii(Alphabet, ASCII) :-
    ASCII is Alphabet + 64.
    
cipher(["PIGS", "SAND", "MAIL", "DATE", "HEAD",
       "CLAM", "PEAK", "HEAT", "JOYA", "WELL",
       "TOAD", "CARD", "WILL", "TAPE", "LEGS",
       "TREE", "ROAD", "MAID", "SLAB", "ROCK",
       "HAND", "VASE", "SAFE", "CLAY", "TOES"]).

run :-
    %string_to_nums("SWAN", A, B, C, D),
    %writeln([A, B, C, D]), % [19,23,1,14]
    %string_to_nums("SONG", E, F, G, H),
    %writeln([E, F, G, H]), % [19,15,14,7]
    %find_core(1923, 114, 1915, 147, SWAN),
    %writeln(SWAN),         % no dice

    % M CC XI II
    find_core(1000, 200, 11, 2, FamilyCore),
    writeln(FamilyCore),
    cipher(Cipher),
    maplist(string_to_core, Cipher, Out),
    maplist(alphabet_to_ascii, Out, Out2),
    string_codes(Ans, Out2),
    writeln(Ans).
