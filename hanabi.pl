% welcome to HANABI - fireworks game where you feel dumb af

% In the before times...
% Before there was VSCode LiveShare or Discord
% We interacted in REAL LIFE!
% (oprobe_time(OldTime)h, scary)

% Long ago, the four nations lived together in harmony
% Then, everything changed when the Sudoku Nation attacked
% Only the Avatar, master of all four elements, could stop them
% But when the world needed him most, he couldn't solve a puzzle

/*
variables storing data in a recursive relation
- state: Played, Discarded, Deck, Tokes, Misses, Hands

card(Col, Num, (ColKnown, NumKnown))
- Col = red ; yellow ; green ; blue ; white .

*/

:- include(frontend).
:- use_module(library(random)).

% src :- https://stackoverflow.com/questions/32691313/counter-in-prolog

:- dynamic played_cards/1.
:- dynamic information_token/1.
:- dynamic miss_chances/1.
:- dynamic is_count_down/1.
:- dynamic countdown_to_end_game/1.

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

% decrements the number of information tokens by 1
dec_information_token :-
    information_token(OldTokens),
    OldTokens > 0,
    succ(NewTokens, OldTokens),
    retractall(information_token(_)),
    assertz(information_token(NewTokens)).

% increments the number of information tokens by 1
inc_information_token :-
    information_token(OldTokens),
    OldTokens < 8,
    succ(OldTokens, NewTokens),
    retractall(information_token(_)),
    assertz(information_token(NewTokens)).

% 3 chances to miss
init_miss_chances :-
    retractall(miss_chances(_)),
    assertz(miss_chances(3)).

dec_miss_chances :-
    miss_chances(OldChances),
    succ(NewChances, OldChances),
    retractall(miss_chances(_)),
    assertz(miss_chances(NewChances)).

% done
init_is_count_down :- 
    retractall(is_count_down(_)),
    assertz(is_count_down(0)).

activate_is_count_down :- 
    retractall(is_count_down(_)),
    assertz(is_count_down(1)).

% done
init_countdown_to_end_game :- 
    retractall(countdown_to_end_game(_)),
    assertz(countdown_to_end_game(0)).

activate_countdown_to_end_game :- 
    retractall(countdown_to_end_game(_)),
    assertz(countdown_to_end_game(3)).

dec_countdown_to_end_game :- 
    countdown_to_end_game(OldCountdown),
    succ(NewCountdown, OldCountdown),
    retractall(countdown_to_end_game(_)),
    assertz(countdown_to_end_game(NewCountdown)).

is_field_complete :-
    played_cards(card(red, 5, (_, _))),
    played_cards(card(blue, 5, (_, _))),
    played_cards(card(green, 5, (_, _))),
    played_cards(card(yellow, 5, (_, _))),
    played_cards(card(white, 5, (_, _))).

score_played_field(Score) :- 
    Count is 0,
    played_cards(card(red, RedCount, (_, _))),
    Count1 is Count + RedCount,
    played_cards(card(blue, BlueCount, (_, _))),
    Count2 is Count1 + BlueCount,
    played_cards(card(green, GreenCount, (_, _))),
    Count3 is Count2 + GreenCount,
    played_cards(card(yellow, YellowCount, (_, _))),
    Count4 is Count3 + YellowCount,
    played_cards(card(white, WhiteCount, (_, _))),
    Score is Count4 + WhiteCount.

% identity(A, B) is true is A and B are the same
identity(X, X).

% rotates the player turn order clockwise
rotate_players(state(Player, team(P2, P3, P4), Deck, Discard), state(P2, team(P3, P4, Player), Deck, Discard)).

% valid_play is true if card(C, N0) can be played without causing a miss
valid_play(card(Col, N0, _)) :-
    played_cards(card(Col, N1, (_,_))),
    N1 is N0-1.
old_valid_play(card(Col, N0, _), Played) :-
    member(card(Col, N1, _), Played),
    N1 is N0-1.

% valid_clue(Clue) is true if Clue is a valid clue to give
valid_clue(1).
valid_clue(2).
valid_clue(3).
valid_clue(4).
valid_clue(5).
valid_clue(red).
valid_clue(yellow).
valid_clue(green).
valid_clue(blue).
valid_clue(white).

% convert_to_clue(S, Clue) is true if S is the tring representation of a Clue
convert_to_clue('red', red).
convert_to_clue('yellow', yellow).
convert_to_clue('green', green).
convert_to_clue('blue', blue).
convert_to_clue('white', white).

% parse_clue(Clue, Parsed) is true if Parsed is a valid clue parsed from Clue
%parse_clue(Clue, Parsed) :- number_string(Parsed, Clue), number(Parsed), valid_clue(Parsed).
parse_clue(Clue, Clue) :- valid_clue(Clue).
parse_clue(Clue, Parsed) :- string_lower(Clue, LC), convert_to_clue(LC, Parsed), valid_clue(Parsed).

% hand_has_clueable(Clue, Hand) is true if at least one of the cards in Hand can be clued with Clue
hand_has_clueable(Col, [card(Col, _, _)|_]).
hand_has_clueable(Num, [card(_, Num, _)|_]).
hand_has_clueable(X, [card(Col, Num, _)|T]) :-
    dif(Col, X), dif(Num, X), hand_has_clueable(X, T).

% give_clue(Clue, Hand, NewHand, CluedIndices) marks all cards in Hand that have the clued property as being clued
    % true if NewHand is the updated hand, and CluedIndices is a list of the indices of the clued cards
give_clue(Clue, Hand, NewHand, CluedIndices) :-
    give_clue_helper(Clue, Hand, 1, NewHand, CluedIndices).

% recursive helper for give_clue
give_clue_helper(_, [], _, [], []).
give_clue_helper(Col, [card(Col, Num, (_, NK))|Rest], I, [card(Col, Num, (true, NK))|NewRest], [I|CluedIndices]) :-
    I1 is I+1,
    give_clue_helper(Col, Rest, I1, NewRest, CluedIndices).
give_clue_helper(Num, [card(Col, Num, (CK, _))|Rest], I, [card(Col, Num, (CK, true))|NewRest], [I|CluedIndices]) :-
    I1 is I+1,
    give_clue_helper(Num, Rest, I1, NewRest, CluedIndices).
give_clue_helper(X, [card(Col, Num, (CK, NK))|Rest], I, [card(Col, Num, (CK, NK))|NewRest], CluedIndices) :-
    dif(X, Col), dif(X, Num),
    I1 is I+1,
    give_clue_helper(X, Rest, I1, NewRest, CluedIndices).

% plays a card on the field, then gives the player a new card from the draw deck
play_card(I, Hand, Deck, Discard, NewHand, NewDeck, Discard) :-
    get_nth(Hand, I, Card),
    play_card_on_field(Card),
    remove_nth(Hand, I, ReducedHand),
    draw_card(ReducedHand, Deck, NewHand, NewDeck).

% discards a card from a player's hand, then gives the player a new card from the draw deck
discard_card(I, Hand, Deck, Discard, NewHand, NewDeck, [Card|Discard]) :-
    get_nth(Hand, I, Card),
    remove_nth(Hand, I, ReducedHand),
    draw_card(ReducedHand, Deck, NewHand, NewDeck).

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

% Draw top card of the deck into player hand on the left
draw_card(Hand, [H|T], [H|Hand], T).
draw_card(Hand, [], Hand, []).

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

% src :- https://comp.lang.prolog.narkive.com/6BvFqRTX/how-to-delete-nth-element-from-a-list
remove_nth([_|T],1,T).
remove_nth([H|T],N,[H|X]) :- N1 is N-1, remove_nth(T,N1,X).

get_nth([H|_],1,H).
get_nth([_|T],N,X) :- N1 is N-1, get_nth(T,N1,X).

/* 
shuffle_deck([], _, []).
shuffle_deck([H|T], RandNum, [Curr|Return]) :- 
   get_nth([H|T], RandNum, Curr),
   remove_nth([H|T], RandNum, Remain),
   length(Remain, L),
   random_between(1,L,R),
   shuffle_deck(Remain, L, Return).

shuffle_deck_2([], []).
shuffle_deck_2([H|T], [Curr|Return]) :- 
    random_select(Curr, [H|T], Rest),
    shuffle_deck_2(Rest, Return).
*/

shuffle_deck_3([], []).
shuffle_deck_3([H|T], [Curr|Return]) :- 
    length([H|T], L),
    random_between(1,L,R),
    get_nth([H|T], R, Curr),
    remove_nth([H|T], R, Remain),
    shuffle_deck_3(Remain, Return).


starting_deck(Deck) :-
    init_deck(Sorted),
    shuffle_deck_3(Sorted, Deck).

% I'm sorry you had to see this. This is a sin, a crime against humanity.
starting_hands(state(player(_,N1),team(player(_,N2),player(_,N3),player(_,N4)),[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P|Q],Discard), state(player([A,B,C,D],N1),team(player([E,F,G,H],N2),player([I,J,K,L],N3),player([M,N,O,P],N4)),Q,Discard)).

% starting_hands(state(player([A|B|C|D|E],N1),(player([F|G|H|I|J],N2,player([K|L|M|N|O],N3),player([P|Q|R|S|T],N4)),U,Discard))).
check_deck_empty(state(_,_,[],_)).
