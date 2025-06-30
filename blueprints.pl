%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WARNING: MAJOR SPOILERS! %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 45 rooms labelled A1 (bottom left) through I5 (top right).
% lantern colors: (r)ed, (g)reen, (y)ellow, (b)lue, (v)iolet, (o)range

/*
--------------------------------------
|   o      o  |      |   v      r    |
----  -- --  --------- ------ --  -- -
| |    | |    | |    | |    | |    | |
|r| I1  r| I2  o  I3  v| I4  v  I5  r|
| |    | |    | |    | |    | |    | |
- --  -- --  -- --  -- ------ ------ -
|   o      r  |   r    | v      y    |
- ------ --  -- ------ --  --------- -
| |    | |    | |    | |    | |    | |
|r  H1  o  H2  r| H3 |y| H4  v  H5 |r|
| |    | |    | |    | |    | |    | |
-------- ---------  -- ------ ------ -
|   r      r      r      r      v    |
----  -- ------------- ------ ------ -
| |    | |    | |    | |    | |    | |
|r  G1  o| G2  y| G3 |y  G4  r  G5  v|
| |    | |    | |    | |    | |    | |
-------- ------ --  -----  -- --  -- -
|   o    | y    | y      v      v  | |
- --  -----  -- --  -- --  -- ------ -
| |    | |    | |    | |    | |    | |
|o| F1  v  F2  y  F3  v| F4 |r| F5  v|
| |    | |    | |    | |    | |    | |
- --  -----  --------- ------ --  -- -
|   v    | v      v      r      r    |
- ------ ------------- ------ ------ -
| |    | |    | |    | |    | |    | |
|o| E1  v  E2 |o  E3  o  E4  v  E5  r|
| |    | |    | |    | |    | |    | |
- --  -- --  -- --  -- ------ ------ -
|   o      o    | r      y    | y    |
----  -- ------ -------------------- -
| |    | |    | |    | |    | |    | |
|g  D1 |v  D2 |g  D3  o  D4 |v| D5 |r|
| |    | |    | |    | |    | |    | |
- ------ --  --------- --  -- --  -- -
|   v    | g      o      v      v    |
- ----------------  -- --  -----  -- -
| |    | |    | |    | |    | |    | |
|v  C1 |g  C2  o  C3  r| C4  r  C5 |r|
| |    | |    | |    | |    | |    | |
- ------ --  -- ------ ------ --  -- -
|   v      v      o      o      o    |
----  -----  -- --  -- --  -- --  -- -
| |    | |    | |    | |    | |    | |
|g| B1  g| B2  y| B3 |y| B4  r  B5  r|
| |    | |    | |    | |    | |    | |
- ------ --  -- --  -- ------ --  -- -
|    g     v  |   o      r      o    |
- --  -----  -- --  -- --  -- ------ -
| |    | |    | |    | |    | |    | |
|g| A1  v  A2 |y  A3  r| A4  b| A5  o|
| |    | |    | |    | |    | |    | |
- ------ ------ ------ ------ ------ -
|                                    |
------------------  ------------------

_ H _ S E	THROUGH LANTERN LIGHT IN SKETCHES
_ _ _ N G	CAST A TINT OF TRUTH
_ L _ N _	TO FIND OUR PATH A
F _ R G _	HUE OF WISDOM TO TURN
B _ R O N	US RIGHT A SHADE OF
C R _ S T	RAIN TO PASS THE LIGHT
_ M O _ _	THROUGH LANTERN LIGHT IN SHADOWS
_ E W A M	PAST WE SEEK WHAT'S LEFT
B L E S T	OF THE LIES WE CAST
*/

% A1: conservatory. (B)EAR - EAR. OF.
puzzle(conservatory, green, [[black,  gray,  orange],
                             [orange, green, orange],
                             [orange, green, yellow]]).

% A2: maid's chamber. C(L)OG - COG. THE.
puzzle(maids_chamber, violet, [[pink,   pink,   pink],
                               [orange, orange, orange],
                               [violet, violet, violet]]).

% A3: entrance hall. CAN - CAN(E). LIES.
puzzle(entrance_hall, yellow, [[yellow, yellow, yellow],
                               [black,  yellow, green],
                               [gray,   blue,   gray]]).

% A4: pantry. spear and harp, maybe (S)HARP - HARP ? WE.
puzzle(pantry, blue, [[blue,  violet, blue],
                      [black, blue,   violet],
                      [blue,  violet, violet]]).

% A5: storeroom. (T)HORN - HORN. CAST.
puzzle(storeroom, red, [[black, black,  black],
                        [gray,  orange, gray],
                        [pink,  red,    pink]]).

% B1: bedroom. arch/trellis/pergola-ship/port/harbor?? PAST.
puzzle(bedroom, green, [[orange, yellow, orange],
                        [green,  gray,   green],
                        [blue,   green,  blue]]).

% B2: drawing room. PI(E) - PI. WE.
puzzle(drawing_room, violet, [[violet, violet, violet],
                              [black,  gray,   gray],
                              [orange, blue,   orange]]).

% B3: gallery. (W)EIGHT-EIGHT. SEEK.
puzzle(gallery, blue, [[gray,   black, gray],
                       [yellow, blue,  yellow],
                       [blue,   blue,  blue]]).

% B4: library. BE(A)DS-BEDS. WHAT'S
puzzle(library, blue, [[pink,   orange, orange],
                       [blue,   blue,   pink],
                       [orange, orange, pink]]).

% B5: courtyard. CO(M)B-COB. LEFT.
puzzle(courtyard, [orange, red, red, orange], [[pink, black, pink],
                                               [orange, gray, orange],
                                               [pink, red, pink]]).

% C1: solarium. moon tarot-horse? THROUGH
puzzle(solarium, green, [[black,  green,  black],
                         [orange, violet, orange],
                         [blue,   green,  blue]]).

% C2: hallway. LA(M)B - LAB. LANTERN.
puzzle(hallway, blue, [[white,  white, white],
                       [yellow, white, black],
                       [blue,   blue,  blue]]).

% C3: dining room. fruit-farm, maybe (O)RANGE - RANGE(?). LIGHT.
puzzle(dining_room, orange, [[pink,   orange, pink],
                             [orange, blue,   orange],
                             [pink,   orange, pink]]).

% C4: observatory. shoelace-spear? IN
puzzle(observatory, blue, [[green,  blue, gray],
                           [black,  blue, gray],
                           [violet, blue, blue]]).

% C5: east wing hallway. cog/gear - corn/maize? SHADOWS.
puzzle(east_wing_hallway, blue, [[violet, white, violet],
                                 [white,  white, white],
                                 [blue,   pink,  blue]]).

% D1: nook. rook-cane? ROOK-(C)ROOK (?). RAIN.
puzzle(nook, violet, [[red,    green, orange],
                      [black,  black, black],
                      [violet, blue, violet]]).

% D2: kitchen. PIE(R)-PIE. TO.
puzzle(kitchen, orange, [[orange, green,  orange],
                         [yellow, orange, orange],
                         [blue,   yellow, orange]]).

% D3: corridor. spear-kickboxing? PASS.
puzzle(corridor, blue, [[green, yellow, green],
                        [blue,  orange, blue],
                        [blue,  orange, blue]]).

% D4: music room. signs-flame. PO(S)T-POT (?). THE.
puzzle(music_room, blue, [[pink, blue,  blue],
                          [blue, blue,  violet],
                          [green, gray, pink]]).

% D5: lavatory. horse-seedling? S(T)EED-SEED(?) LIGHT.
puzzle(lavatory, [green, yellow, yellow, green], [[orange, yellow, orange],
                                                  [green,  yellow, green],
                                                  [black,  yellow, black]]).

% E1: billiard room. (B)ONE-ONE. US.
puzzle(billiard_room, red, [[pink, white, pink],
                            [blue, gray,  blue],
                            [pink, red,   pink]]).

% E2: trophy room. ship/port-suitcase? RIGHT.
puzzle(trophy_room, white, [[white, red,  gray],
                            [gray,  pink, gray],
                            [black, red,  white]]).

% E3: the foundation. river-flaming pot? ST(R)EAM-STEAM(?) A.
puzzle(foundation, orange, [[gray,   orange, orange],
                            [yellow, orange, black],
                            [blue,   blue,   orange]]).

% E4: ballroom. pig/swine/boar-music/note/bar? B(O)AR-BAR(?). SHADE.
puzzle(ballroom, blue, [[green, white, white],
                        [black, white, black],
                        [blue,  white, blue]]).

% E5: spare room. SAIL-S(N)AIL. OF.
puzzle(spare_room, blue, [[black, blue,   gray],
                          [blue,  orange, blue],
                          [black, orange, yellow]]).

% F1: west wing hallway. (F)ARM-ARM. HUE.
puzzle(west_wing_hallway, violet, [[black,  green,  black],
                                   [gray,   orange, gray],
                                   [violet, violet, violet]]).

% F2: passageway. pen-can? OF.
puzzle(passageway, violet, [[pink,   white, pink],
                            [orange, red,   orange],
                            [violet, white, violet]]).
/*
% solved by hand cause it took too long
% below is after starting with red in the middle.
% afterwards, red should never ever be pressed, so swap it out for gray.
% this still takes a long time but reaches a solution at least!
puzzle(passageway, violet, [[pink,   black, pink],
                            [orange, gray,   orange],
                            [violet, black, violet]]).
*/

% F3: darkroom. BOOK-B(R)OOK. WISDOM.
puzzle(darkroom, orange, [[yellow, orange, yellow],
                          [orange, orange, orange],
                          [pink,   orange, pink]]).

% F4: closet. PI(G)-PI. TO.
puzzle(closet, orange, [[pink,   orange, pink],
                        [orange, orange, orange],
                        [green,  orange, green]]).

% F5: parlor. rat-chart?
puzzle(parlor, green, [[black, blue,  black],
                       [black, green, orange],
                       [blue,  green, blue]]).

% G1: gymnasium. medal-shoelace? TO.
puzzle(gymnasium, red, [[pink,  white, pink],
                        [black, red,   black],
                        [gray,  gray,  gray]]).

% G2: laundromat. clip-ship? P(L)IER-PIER(?). FIND.
puzzle(laundromat, violet, [[pink,   orange, violet],
                            [orange, gray,   gray],
                            [pink,   blue,   violet]]).

% G3: guest bedroom. thorn-flaming pan? pan-pain? OUR.
puzzle(guest_bedroom, [black, yellow, yellow, black], [[yellow, yellow, yellow],
                                                       [black,  black,  black],
                                                       [gray,   green,  gray]]).

% G4: conference room. farm-music. BAR(N)-BAR(?). PATH.
puzzle(conference_room, yellow, [[yellow, yellow, yellow],
                                 [black,  gray,   black],
                                 [orange, green,  orange]]).

% G5: den. spear-chart/graph/peak? A.
puzzle(den, violet, [[gray, pink, gray],
                     [orange, green, orange],
                     [violet, gray, violet]]).

% H1: foyer. book-christmas tree? pine-spine? CAST.
puzzle(foyer, green, [[green, green,  green],
                      [gray,  orange, orange],
                      [blue,  gray,   violet]]).

% H2: the pool. calendar/paper/eight-ear? A.
puzzle(pool, red, [[red,  white,  yellow],
                   [blue, green,  blue],
                   [blue, yellow, blue]]).

% H3: servant's quarters. earth worm-baseball bat. TINT.
puzzle(servants_quarters, black, [[gray,  gray, gray],
                                  [black, red,  black],
                                  [white, gray, yellow]]).

% H4: pump room. PI-PI(N). OF.
puzzle(pump_room, violet, [[violet, blue, violet],
                           [black,  green, orange],
                           [black,  green, orange]]).
% solved by hand. blue+orange explodes the options

% H5: furnace. (G)LOBE-(ear)LOBE (?). TRUTH.
puzzle(furnace, blue, [[black, gray,   gray],
                       [white, green,  white],
                       [blue,  yellow, orange]]).
% solved by hand.

% I1: archive. horse-banana? THROUGH.
puzzle(archive, [orange, red, red, orange], [[red,    gray,   black],
                                             [orange, orange, orange],
                                             [green,  gray,   violet]]).

% I2: chapel. three/weight-christmas tree T(H)REE-TREE(?). LANTERN.
puzzle(chapel, orange, [[blue, orange, blue],
                        [black, black, orange],
                        [blue, orange, blue]]).

% I3: vestibule. wine bottle-medal. LIGHT.
puzzle(vestibule, [black, violet, orange, red], [[gray,   green, gray],
                                                 [orange, black, red],
                                                 [black,  white, violet]]).
/*
% solving the above takes way too long.
% observation: orange should _never_ be clicked.
puzzle(vestibuletest, [black, violet, test, red], [[gray,   green, gray],
                                                   [test, black, red],
                                                   [black,  white, violet]]).
*/

% I4: coat check. PEAR-(S)PEAR. IN.
puzzle(coat_check, [blue, black, black, blue], [[yellow, green, yellow],
                                                [black,  black, black],
                                                [blue,   green, blue]]).

% I5: aquarium. L(E)ASH-LASH. SKETCHES.
% solved by hand. There are a couple of sub-goals that make this one easier:
% two yellow are missing and only orange/blue can turn into them.
% at least once, we need orange in the middle with blue adjacent to majority orange.
puzzle(aquarium, yellow, [[pink,   gray,   yellow],
                          [red,    yellow, white],
                          [orange, blue,   black]]).

% Puzzle solution: room 46! Box solution: BLUE.
% note: solution has some weird spurious steps!
puzzle(room46, blue, [[orange, pink,  orange],
                      [gray,   green, gray],
                      [blue,   gray,  blue]]).

% route traces the word SWANSONG and the phrase WE SEEK WHAT'S IN THE SHADE OF TRUTH
