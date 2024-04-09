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

library(statistics).

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

    read_term(Ans, [syntax_errors(dec10)]), nl,
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
    writeln('Detailed rules found in the Wikipedia article, or in the rulebook'),
    writeln('https://en.wikipedia.org/wiki/Hanabi_(card_game)'), 
    writeln('https://cdn.1j1ju.com/medias/b3/a9/0e-hanabi-rulebook.pdf'), nl,
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
    writeln('Starting up the game... make sure you have all 4 people present and mark yourselves as players A, B, C and D'), nl, nl,
    init_information_token, init_miss_chances, init_played_cards, init_is_count_down, init_countdown_to_end_game,
    starting_deck(Deck),
    starting_hands(state(player([], 'Player A'), team(player([], 'Player B'), player([], 'Player C'), player([], 'Player D')), Deck, []), NewState),
    gameplay_loop(NewState).

gameplay_loop(state(Player, team(P2, P3, P4), Deck, Discard)) :- 
    confirm_player_ready(Player),
    print_info(state(Player, team(P2, P3, P4), Deck, Discard)),
    handle_player_action(state(Player, team(P2, P3, P4), Deck, Discard), NewState),
    ((check_deck_empty(NewState), is_count_down(Active), Active is 0) ->
        writeln('You have drawn the last card in the deck! Each player will get 1 more turn before the game ends') ; write('')),
    confirm_player_done(Player),
    handle_round_end(NewState).

handle_round_end(State) :-
    miss_chances(Miss),
    (Miss =< 0 ->
        writeln('You have misfired three times, ending the game!'),
        end_game_scoring ;
        (is_field_complete ->
            end_game_scoring ;
            is_count_down(Active),
            ((countdown_to_end_game(N), Active is 1, N is 0) ->
                writeln('The fireworks show is over (since the deck ran out on your last turn), ending the game!'),
                end_game_scoring ;
                (Active is 1 ->
                    dec_countdown_to_end_game ;
                    (check_deck_empty(State) ->
                        activate_countdown_to_end_game, activate_is_count_down ;
                        true)),
                rotate_players(State, NewState),
                clear_screen,
                gameplay_loop(NewState)
            ))).

end_game_scoring :-
    score_played_field(Score),
    format('Your team scored ~d points!~n', [Score]),
    writeln('--- GAME OVER ---').

clear_screen :-
    % TEMP: just a bunch of newlines haha
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    format('~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n'),
    writeln('--- INFO FROM PREVIOUS TURNS HIDDEN ---'), nl.

% Confirms that a player is ready to take their actions
confirm_player_ready(player(Hand, Name)) :-
    write('Hey, make sure only '), write(Name), write(' is looking at the screen.'), nl,
    writeln('Type "okay." when you are ready!'),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (Ans = 'okay' -> 
         writeln('Sounds good, champ!'), nl;
         writeln('Try again!'),
         confirm_player_ready(player(Hand, Name))).

% Confirms that a player is ready to proceed to the next turn
confirm_player_done(player(Hand, Name)) :-
    write('Type "done." to end your turn.'), nl,
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (Ans = 'done' -> 
         writeln('Sounds good, champ!'), nl;
         writeln('Try again!'),
         confirm_player_done(player(Hand, Name))).

% state(Player, team(P2, P3, P4), Deck, Discard)

init_turn_timer(TimerId) :- 
    writeln('Started the timer, you have 60 seconds!'),
    thread_create(turn_timer(true,0), TimerId).

turn_timer(IsFirstCall, InitialTime) :- 
    sleep(1),
    statistics(real_time, [RealTime,_]),
    (IsFirstCall -> 
        turn_timer(false,RealTime);
        TimeDiff is RealTime - InitialTime,
        (TimeDiff = 30 ->
            writeln('Thirty seconds remaining!'),
            turn_timer(false,InitialTime);
            (TimeDiff = 50 ->
                writeln('Ten seconds remaining!'),
                turn_timer(false,InitialTime);
                (TimeDiff >= 60 ->
                    writeln('Time''s up!') ;
                    turn_timer(false,InitialTime))))).

check_timer(TimerId) :-
    catch(
        thread_signal(TimerId, exit),
        error(_, _Context),
        (writeln('You took too long to act, so you automatically misfired.'), nl,
        false)
    ),
    true.

reset_timer(TimerId) :-
    catch(
        thread_signal(TimerId, exit),
        error(_, _Context),
        (writeln('Resetting the timer!'), nl,
        false)
    ),
    true.

handle_player_action(State, NewState) :-
    information_token(Tokens),
    writeln('Which action would you like to take?'),
    writeln('[1] Play a card onto the playfield'),
    (Tokens < 8 -> writeln('[2] Discard a card to gain a clue token') ; write('')),
    (Tokens > 0 -> writeln('[3] Give another player a clue') ; write('')),
    init_turn_timer(TimerId),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (check_timer(TimerId) ->
        (check_ans_action(Ans, Tokens) ->
            (Ans = 1 ->
                handle_play(State, NewState) ;
                (Ans = 2 ->
                    handle_discard(State, NewState) ;
                    handle_clue(State, NewState))) ;
            (reset_timer(TimerId),
            invalid_handle_player_action(State, NewState))) ;
        dec_miss_chances, identity(State, NewState)).

invalid_handle_player_action(State, NewState) :-
    writeln('Invalid input, please re-enter.'),
    nl,
    handle_player_action(State, NewState).

check_ans_action(1, _).
check_ans_action(2, X) :- dif(X, 8).
check_ans_action(3, X) :- dif(X, 0).

handle_play(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)) :-
    writeln('Which card would you like to play?'),
    print_playable_cards(Hand), 
    init_turn_timer(TimerId),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (check_timer(TimerId) ->
        ((number(Ans), length(Hand, L), Ans > 0, Ans =< L) ->
            get_nth(Hand, Ans, card(Col, Num, Clues)),
            write('You play a '), print_card(card(Col, Num, Clues)),
            (valid_play(card(Col, Num, Clues)) ->
                % PASS: play
                writeln(' and SUCCEED'),
                play_card(Ans, Hand, Deck, Discard, NewHand, NewDeck, NewDiscard),
                length(NewHand, NewL),
                ((Num = 5, information_token(Tokens), Tokens < 8) ->
                    writeln('You successfully played a 5, so you gain an information token'),
                    inc_information_token ; write('')),
                (NewL < 4 ->
                    writeln('The deck is empty, so you do not draw a new card') ;
                    writeln('You draw a new card from the deck')) ;
                % FAIL: misfire and discard
                writeln(' and MISFIRE'), dec_miss_chances,
                discard_card(Ans, Hand, Deck, Discard, NewHand, NewDeck, NewDiscard),
                length(NewHand, NewL),
                (NewL < 4 ->
                    writeln('The played card is discarded') ;
                    writeln('The played card is discarded, and you draw a new card from the deck'))) ;
            writeln('Invalid input, please re-enter.'), nl,
            handle_play(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)) ;
        dec_miss_chances, identity(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)))).

handle_discard(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard)) :-
    writeln('Which card would you like to discard?'),
    print_playable_cards(Hand), 
    init_turn_timer(TimerId),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (check_timer(TimerId) ->
        (number(Ans), length(Hand, L), Ans > 0, Ans =< L ->
            % PASS: discard card
            get_nth(Hand, Ans, Card), inc_information_token,
            discard_card(Ans, Hand, Deck, Discard, NewHand, NewDeck, NewDiscard),
            write('You discard a '), print_card(Card), writeln(' and gain an information token'),
            length(NewHand, NewL),
            (NewL < 4 ->
                writeln('The deck is empty, so you do not draw a new card') ;
                writeln('You draw a new card from the deck')) ;
            % FAIL: try again
            writeln('Invalid input, please re-enter.'), nl,
            handle_discard(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard))) ;
        dec_miss_chances, identity(state(player(Hand, Name), Team, Deck, Discard), state(player(NewHand, Name), Team, NewDeck, NewDiscard))).

handle_clue(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), NewState) :-
    writeln('Which player would you like to give a clue to?'),
    format('[1] ~s: ', [N2]), print_hand_with_clues(H2), nl,
    format('[2] ~s: ', [N3]), print_hand_with_clues(H3), nl,
    format('[3] ~s: ', [N4]), print_hand_with_clues(H4), nl,
    init_turn_timer(TimerId),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (check_timer(TimerId) ->
        ((number(Ans), Ans >= 1, Ans =< 3) ->
            % PASS: choose a clue to give
            handle_clue_selection(Ans, state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), NewState) ;
            % FAIL: try again
            writeln('Invalid input, please re-enter.'), nl,
            handle_clue(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), NewState)) ;
        dec_miss_chances, identity(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), NewState)).

handle_clue_selection(ChosenPlayer, state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard), state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)) :-
    writeln('The player must have at least 1 card with the clued propery in order for the clue to be valid'),
    writeln('What clue would you like to give? (one of {red, yellow, green, blue, white, 1, 2, 3, 4, 5})'),
    % writeln('[back] Choose a different player to give a clue'),
    init_turn_timer(TimerId),
    read_term(Ans, [syntax_errors(dec10)]), nl,
    (check_timer(TimerId) ->
        (parse_clue(Ans, Parsed) ->
            ((ChosenPlayer = 1, hand_has_clueable(Parsed, H2)) ->
                give_clue(Parsed, H2, NewH2, _), dec_information_token,
                identity(H3, NewH3), identity(H4, NewH4),
                format('You have spent an information token to give ~s the clue ''~w''~n', [N2, Parsed]) ;
                ((ChosenPlayer = 2, hand_has_clueable(Parsed, H3)) ->
                    give_clue(Parsed, H3, NewH3, _), dec_information_token,
                    identity(H2, NewH2), identity(H4, NewH4),
                    format('You have spent an information token to give ~s the clue ''~w''~n', [N3, Parsed]) ;
                    ((ChosenPlayer = 3, hand_has_clueable(Parsed, H4)) ->
                        give_clue(Parsed, H4, NewH4, _), dec_information_token,
                        identity(H2, NewH2), identity(H3, NewH3),
                        format('You have spent an information token to give ~s the clue ''~w''~n', [N4, Parsed]) ;
                        (writeln('Invalid input, please re-enter.'), nl,
                        handle_clue_selection(ChosenPlayer, state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
                            state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)))
                        )))
            ;
            (writeln('Invalid input, please re-enter.'), nl,
            handle_clue_selection(ChosenPlayer, state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
                state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard)))) ;
            dec_miss_chances, identity(state(Player, team(player(H2, N2), player(H3, N3), player(H4, N4)), Deck, Discard),
                state(Player, team(player(NewH2, N2), player(NewH3, N3), player(NewH4, N4)), Deck, Discard))).

% Shows the player all of the cards in their hand that they can play
print_playable_cards(Hand) :- print_playable_cards_helper(Hand, 1).

print_playable_cards_helper([], _).
print_playable_cards_helper([card(Col, Num, (CK, NK))|T], I) :-
    format('[~d] ', [I]), 
    print_hidden_card(card(Col, Num, (CK, NK))), nl,
    I1 is I+1, print_playable_cards_helper(T, I1).

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

%%%%%%%%%%%%%%%
%% HAMBURGER %%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
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
print_info(state(player(H1, _), team(player(H2, P2), player(H3, P3), player(H4, P4)), Deck, Discard)) :-
    write(P2), write('''s hand: ['), print_hand_with_clues(H2), write(']'), nl,
    write(P3), write('''s hand: ['), print_hand_with_clues(H3), write(']'), nl,
    write(P4), write('''s hand: ['), print_hand_with_clues(H4), write(']'), nl,
    write('Your hand: ['), print_hidden_hand(H1), write(']'), nl, nl,
    information_token(Tokens),
    miss_chances(M), Miss is 3-M,
    length(Deck, L),
    format('~d information tokens, ~d misfire', [Tokens, Miss]),
    (dif(Miss, 1) -> write('s') ; write('')), 
    format(', ~d card', [L]),
    (dif(L, 1) -> write('s') ; write('')),
    writeln(' left in deck'),
    ((is_count_down(Active), Active is 1) ->
        writeln('The draw deck is empty, so this is your last turn!') ; write('')),
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
        (dif(Red, 0)    -> stringify(red, RS),    format('~s-~d ', [RS, Red]) ; true),
        (dif(Yellow, 0) -> stringify(yellow, YS), format('~s-~d ', [YS, Yellow]) ; true),
        (dif(Green, 0)  -> stringify(green, GS),  format('~s-~d ', [GS, Green]) ; true),
        (dif(Blue, 0)   -> stringify(blue, BS),   format('~s-~d ', [BS, Blue]) ; true),
        (dif(White, 0)  -> stringify(white, WS),  format('~s-~d ', [WS, White]) ; write(''))
    ), nl.

print_discarded_cards(Cards) :-
    write('Discarded: '),
    (dif(Cards, []) ->
        print_hand(Cards) ;
        write('[none]')
    ), nl.

print_hand_with_clues([]).
print_hand_with_clues([card(Col, Num, (CK, NK))|T]) :-
    print_card(card(Col, Num, (CK, NK))), write(' \50'), print_hidden_card(card(Col, Num, (CK, NK))), write('\51'),
    (dif(T, []) ->
        write(', '), print_hand_with_clues(T) ;
        write('')).

print_hand([]).
print_hand([card(Col, Num, _)|T]) :-
    print_card(card(Col, Num, _)),
    (dif(T, []) ->
        write(', '), print_hand(T) ;
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