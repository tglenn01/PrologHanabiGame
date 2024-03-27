% welcome to HANABI - fireworks game where you feel dumb af

% In the before times...
% Before there was VSCode LiveShare or Discord
% We interacted in REAL LIFE!
% (ooh, scary)

% Long ago, the four nations lived together in harmony
% Then, everything changed when the Sudoku Nation attacked
% Only the Avatar, master of all four elements, could stop them
% But when the world needed him most, he couldn't solve a puzzle

/*
variables storing data in a recursive relation
- state: Played, Discarded, Tokens, Misses, Hands
*/

% valid_play is true if card(C, N0) can be played without causing a miss
valid_play(card(C, N0), Played) :-
    member((C, N1), Played),
    N1 is N0-1.

% replace_pair changes the value of the pair with key K to N, if the pair exists
replace_pair(_, _, [], []).
replace_pair(K, N, [(K, _)|T], [(K, N)|T]).
replace_pair(K, N, [(X, Y)|T], [(X, Y)|NewT]) :-
    dif(K, X),
    replace_pair(K, N, T, NewT).
