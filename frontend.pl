%
%
% Frontend insipired by https://github.com/cjyu98/CPSC-312-Book-Recommender/blob/main/main.pl
%
%
%

/*
card(Col, Num, (ColKnown, NumKnown))
Col = red ; yellow ; green ; blue ; white .

state(Player,Team,Deck,Info)
team(Player,Player,Player)
player(Hand,Name)
info(Tokens,Miss,Played,Discard,HintLog)
*/

% start
% starts the application by loading the knowledge base and starting the main menu
start :-
    writeln('Welcome to Hanabi!'), nl,
    main_menu.

main_menu :-
    writeln('Main Menu'),
    writeln('-----------------'),
    writeln('[1] The Rules of Hanabi'),
    writeln('[2] Start Playing!'),
    writeln('[3] Exit'),

    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Invalid input, please re-enter.'), nl, main_menu)
    ), nl,
    check_ans_main(Ans).

check_ans_main(1) :-
    read_rules,
    main_menu.
check_ans_main(2) :-
    start_game.
check_ans_main(3) :-
    writeln('Have a lovely day ^_^'),
    halt.
check_ans_main(Ans) :-
    \+ member(Ans, [1,2,3]),
    writeln('Invalid input, please re-enter.'), nl, main_menu.

read_rules :- 
    writeln('Detailed rules found on the wikipedia article'),
    writeln('https://en.wikipedia.org/wiki/Hanabi_(card_game)'), nl,
    writeln('Welcome to game of Hanabi!'), nl,
    writeln('TL;DR: Play with 4 friends and work together to play your hands in order'),
    writeln('You each are given a hand of 5 cards, where you cant see your own hand but can see your teammates'),
    writeln('Cards have a color [red, blue, green, yellow, or white] and a number [1-5]'),
    writeln('The gamefield starts with a [Red 0], [Green 0], [Yellow 0], [Blue 0], and a [White 0]'),
    writeln('Your goal as a team is to count each colour to 5 going in order'), nl,
    writeln('During your turn you have 3 options'),
    writeln('[1] Use one of your information tokens to tell your teammates information about their hand'),
    writeln('[2] Play a card onto the field (blind!) and guess that you are making a valid move'),
    writeln('[3] Discard one of your cards to gain an information token'), nl, nl.

start_game :-
    writeln('Starting up the game... make sure you have all 4 people present and number yourselves 1-4'), nl, nl,
    % TODO: player name input?
    init_information_token, init_miss_chances, init_played_cards,
    starting_deck(Deck),
    % TODO: deal cards
    starting_hands(state(player([], 'Player 1'), team(player([], 'Player 2'), player([], 'Player 3'), player([], 'Player 4')), Deck, []), NewState),
    gameplay_loop(NewState).

gameplay_loop(state(Player, team(P2, P3, P4), Deck, Discard)) :- 
    confirm_player_ready(Player),
    print_info(state(Player, team(P2, P3, P4), Deck, Discard)),
    handle_player_action(state(Player, team(P2, P3, P4), Deck, Discard), NewState),
    % TODO: handle action, update game state after action
    % dec_miss_chances,
    miss_chances(Miss),
    (dif(Miss, 0) ->
        rotate_players(NewState, NextPlayerState),
        gameplay_loop(NextPlayerState) ;
        writeln('GAME OVER (3 misses)')
        % TODO: game over sequence
    ).

rotate_players(state(Player, team(P2, P3, P4), Deck, Discard), state(P2, team(P3, P4, Player), Deck, Discard)).

% Confirms that a player is ready to take their actions
confirm_player_ready(player(Hand, Name)) :-
    write('Hey, make sure only '), write(Name), write(' is looking at the screen.'), nl,
    writeln('Type "okay." when you are ready!'),
    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Invalid input, please re-enter.'), nl, confirm_player_ready(player(Hand, Name)))
    ), nl,
    (Ans = 'okay' -> 
         writeln('Sounds good, champ!'), nl;
         writeln('Try again!'),
         confirm_player_ready(player(Hand, Name))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START ACTION HANDLING MESS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% state(Player, team(P2, P3, P4), Deck, Discard)

handle_player_action(State, NewState) :-
    information_token(Tokens),
    writeln('Which action would you like to take?'),
    writeln('[1] Play a card onto the playfield'),
    (Tokens < 8 -> writeln('[2] Discard a card to gain a clue token') ; write('')),
    (Tokens > 0 -> writeln('[3] Give another player a clue') ; write('')),
    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Invalid input, please re-enter.'), nl, handle_player_action(State, NewState))
    ), nl,
    (check_ans_action(Ans, Tokens) ->
        (Ans = 1 ->
            handle_play(State, NewState) ;
            (Ans = 2 ->
                handle_discard(State, NewState) ;
                handle_clue(State, NewState))) ;
        writeln('Invalid input, please re-enter.'), nl,
        handle_player_action(State, NewState)).

check_ans_action(1, _).
check_ans_action(2, X) :- dif(X, 8).
check_ans_action(3, X) :- dif(X, 0).

handle_play(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)) :-
    writeln('Which card would you like to play?'),
    print_playable_cards(Hand), 
    writeln('[back] Choose a different action'),
    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Invalid input, please re-enter.'), nl,
        handle_play(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)))
    ), nl,
    (Ans = 'back' ->
        handle_player_action(state(player(Hand, Name), Team, Deck, Discard),
            state(player(NewHand, Name), Team, NewDeck, NewDiscard)) ;
        (number(Ans), length(Hand, L), Ans > 0, Ans =< L ->
            % TODO: handle action
            write('TODO'), not(dif(Hand, NewHand)), not(dif(Deck, NewDeck)), not(dif(Discard, NewDiscard)) ;
            writeln('Invalid input, please re-enter.'), nl,
            handle_play(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)))).

handle_discard(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)) :-
writeln('Which card would you like to discard?'),
print_playable_cards(Hand), 
writeln('[back] Choose a different action'),
catch(
    read(Ans),
    error(syntax_error(_), _),
    (writeln('Invalid input, please re-enter.'), nl,
    handle_discard(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)))
), nl,
(Ans = 'back' ->
    handle_player_action(state(player(Hand, Name), Team, Deck, Discard),
        state(player(NewHand, Name), Team, NewDeck, NewDiscard)) ;
    (number(Ans), length(Hand, L), Ans > 0, Ans =< L ->
        % TODO: handle action
        write('TODO'), not(dif(Hand, NewHand)), not(dif(Deck, NewDeck)), not(dif(Discard, NewDiscard)) ;
        writeln('Invalid input, please re-enter.'), nl,
        handle_discard(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)))).

handle_clue(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)) :-
writeln('Which player would you like to give a clue to?'),
format('[1] ~s: ', [N2]), print_hand(H2), nl,
format('[2] ~s: ', [N3]), print_hand(H3), nl,
format('[3] ~s: ', [N4]), print_hand(H4), nl,
writeln('[back] Choose a different action'),
catch(
    read(Ans),
    error(syntax_error(_), _),
    (writeln('Invalid input, please re-enter.'), nl,
    handle_clue(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
        state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)))
), nl,
(Ans = 'back' ->
    handle_player_action(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
        state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)) ;
    (Ans = 1 ->
        % TODO: handle action for P2
        not(dif(H2, NewH2)), not(dif(H3, NewH3)), not(dif(H4, NewH4)) ;
        (Ans = 2 ->
            % TODO: handle action for P3
            not(dif(H2, NewH2)), not(dif(H3, NewH3)), not(dif(H4, NewH4)) ;
            (Ans = 3 ->
                % TODO: handle action for P4
                not(dif(H2, NewH2)), not(dif(H3, NewH3)), not(dif(H4, NewH4)) ;
                writeln('Invalid input, please re-enter.'), nl,
                handle_clue(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
                    state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)))))).
        
% Shows the player all of the cards in their hand that they can play
print_playable_cards(Hand) :- print_playable_cards_helper(Hand, 1).

print_playable_cards_helper([], _).
print_playable_cards_helper([card(Col, Num, (CK, NK))|T], I) :-
    format('[~d] ', [I]), 
    print_hidden_card(card(Col, Num, (CK, NK))), nl,
    I1 is I+1, print_playable_cards_helper(T, I1).

% force failure on invalid actions
/*check_ans_action(2, 8) :- check_ans_action(false, false).
check_ans_action(3, 0) :- check_ans_action(false, false).
check_ans_action(Ans, _) :-
    \+ member(Ans, [1,2,3]),
    writeln('Invalid input, please re-enter.').*/

% DO NOT DRAW A CARD IF DECK IS EMPTY
/*handle_action(Action, state(player(H1, P1), team(player(H2, P2), player(H3, P3), player(H4, P4)), _, Discard), NewState) :-
    (Action = '1' ->
        (valid_play())
        ;
        (Action = '2' -> 
            
            ;
            (Action = '3' ->

                ;
                dec_miss_chances,
                writeln('No action was taken within 60 seconds, so you have automatically misfired a rocket'))
        )
    ). */

/*
1: play (check if valid)
- ask what card to play
- if valid
  - update played cards
  - if 5 played, increment clue tokens by 1
- if NOT valid
  - increment miss could by 1
  - discard played card
2: discard
- ask what card to discard
- discard played card
- increment clue tokens by 1
3: give clue
- ask what player to give clue to
- ask what to clue
- give_clue
- decrement clue tokens by 1
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END ACTION HANDLING MESS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%% HAMBURGER %%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%% HAMBURGER %%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% B % U % G % I % N % B % I % O %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% one borger pls% Prints out the current game state
    % print_info(state(player([card(green,3,(false,false)),card(blue,2,(true,false)),card(red,3,(false,true)),card(white,1,(true,true))],'baba'),team(player([card(red,2,(false,true)),card(blue,4,(true,false)),card(yellow,4,(true,true)),card(white,1,(true,true))],'keke'),player([card(yellow,2,(false,true)),card(green,3,(true,false)),card(yellow,1,(true,true)),card(green,1,(true,true))],'jiji'),player([card(white,2,(false,true)),card(red,5,(true,false)),card(white,3,(true,true)),card(white,3,(true,true))],'fofo')),[],[])).
print_info(state(player(H1, _), team(player(H2, P2), player(H3, P3), player(H4, P4)), _, Discard)) :-
    write(P2), write('''s hand: ['), print_hand(H2), write(']'), nl,
    write(P3), write('''s hand: ['), print_hand(H3), write(']'), nl,
    write(P4), write('''s hand: ['), print_hand(H4), write(']'), nl,
    write('Your hand: ['), print_hidden_hand(H1), write(']'), nl, nl,
    information_token(Tokens),
    miss_chances(M), Miss is 3-M,
    format('~d information tokens, ~d miss', [Tokens, Miss]),
    (dif(Miss, 1) -> write('es')), nl,
    played_cards(card(red,    NumR, _)),
    played_cards(card(yellow, NumY, _)),
    played_cards(card(green,  NumG, _)),
    played_cards(card(blue,   NumB, _)),
    played_cards(card(white,  NumW, _)),
    print_played_cards(NumR, NumY, NumG, NumB, NumW),
    print_discarded_cards(Discard), nl.

% Prints out the currently played cards
print_played_cards(Red, Yellow, Green, Blue, White) :-
    write('Played: '),
    (Red = 0, Yellow = 0, Green = 0, Blue = 0, White = 0 ->
        write('[none]') ;
        (Red = 0 ->    stringify(red, RS),    format('~s-~d ', [RS, Red])),
        (Yellow = 0 -> stringify(yellow, YS), format('~s-~d ', [YS, Yellow])),
        (Green = 0 ->  stringify(green, GS),  format('~s-~d ', [GS, Green])),
        (Blue = 0 ->   stringify(blue, BS),   format('~s-~d ', [BS, Blue])),
        (White = 0 ->  stringify(white, WS),  format('~s-~d ', [WS, White]))
    ), nl.

print_discarded_cards(Cards) :-
    write('Discarded: '),
    (dif(Cards, []) ->
        print_hand(Cards) ;
        write('[none]')
    ), nl.

% card(Col, Num, (ColKnown, NumKnown))
print_hand([]).
print_hand([card(Col, Num, _)|T]) :-
    print_card(card(Col, Num, _)),
    (dif(T, []) ->
        write(', '), print_hand(T);
        write('')).

print_hidden_hand([]).
print_hidden_hand([card(Col, Num, (CK, NK))|T]) :-
    print_hidden_card(card(Col, Num, (CK, NK))),
    (dif(T, []) ->
        write(', '), print_hidden_hand(T);
        write('')).

print_card(card(Col, Num, _)) :-
    stringify(Col, S),
    write(S),
    format('-~d', [Num]).

print_hidden_card(card(_, _, (false, false))) :-
    write('???-?').
print_hidden_card(card(_, Num, (false, true))) :-
    format('???-~d', [Num]).
print_hidden_card(card(Col, _, (true, false))) :-
    stringify(Col, S),
    write(S),
    write('-?').
print_hidden_card(card(Col, Num, (true, true))) :-
    print_card(card(Col, Num, (true, true))).

stringify(red, 'RED').
stringify(yellow, 'YELLOW').
stringify(green, 'GREEN').
stringify(blue, 'BLUE').
stringify(white, 'WHITE').

% CARD:  COLOR--NUMBER
% ???-?
% ???-5
% RED-?
% RED-5