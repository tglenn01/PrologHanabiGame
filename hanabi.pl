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
- state: Played, Discarded, Deck, Tokens, Misses, Hands

card(Col, Num, (ColKnown, NumKnown))
- Col = red ; yellow ; green ; blue ; white .

*/

[frontend].

% source https://stackoverflow.com/questions/32691313/counter-in-prolog

:- dynamic played_cards/1.
:- dynamic information_token/1.
:- dynamic miss_chances/1.

init_played_cards :-
    retractall(played_cards(_)),
    assertz(played_cards(card(red, 0, (_, _)))),
    assertz(played_cards(card(blue, 0, (_, _)))),
    assertz(played_cards(card(green, 0, (_, _)))),
    assertz(played_cards(card(yellow, 0, (_, _)))),
    assertz(played_cards(card(white, 0, (_, _)))).

% assumes card is valid (correct card to play)
play_card_on_field(card(Col, Num, (_, _))) :-
    retractall(played_cards(card(Col, _, (_,_)))),
    assertz(played_cards(card(Col, Num, (_,_)))).

% 8 information tokens to start
init_information_token :-
    retractall(information_token(_)),
    assertz(information_token(8)).

dec_information_token :-
    information_token(OldTokens),
    retractall(information_token(_)),
    succ(NewTokens, OldTokens),
    assertz(information_token(NewTokens)).

% 3 chances to miss
init_miss_chances :-
    retractall(miss_chances(_)),
    assertz(miss_chances(3)).

dec_miss_chances :-
    information_token(OldChances),
    retractall(information_token(_)),
    succ(NewChances, OldChances),
    assertz(information_token(NewChances)).


% valid_play is true if card(C, N0) can be played without causing a miss
valid_play(card(C, N0, _), Played) :-
    member(card(C, N1, _), Played),
    N1 is N0-1.

% replace_pair changes the value of the pair with key K to N, if the pair exists
replace_pair(_, _, [], []).
replace_pair(K, N, [(K, _)|T], [(K, N)|T]).
replace_pair(K, N, [(X, Y)|T], [(X, Y)|NewT]) :-
    dif(K, X),
    replace_pair(K, N, T, NewT).

% starting_played([(red, 0), (blue, 0), (yellow, 0), (green, 0), (white, 0)]).
% starting_discard([]).

% TODO
% deal_cards()

%Draw top card of the deck into player hand on the left
draw_card(state(player(hand,Name),Team,[H|T],Info)) :- state(player([H|hand],Name),Team,T,Info).

make_card(Col, Num, card(Col, Num, (false, false))).

make_color_cards(_, [], []).
make_color_cards(Col, [Num|T], [Card|Rest]) :-
    make_card(Col, Num, Card),
    make_color_cards(Col, T, Rest).

make_deck([], _, []).
make_deck([Col|T1], Nums, Return) :-
    make_color_cards(Col, Nums, ColCards),
    make_deck(T1, Nums, Rest),
    append(ColCards, Rest, Return).

init_deck(Deck) :- make_deck([red,blue,green,white,yellow], [1,1,1,2,2,3,3,4,4,5], Deck).
% append(L1, L2, Combined)