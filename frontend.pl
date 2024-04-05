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
        (writeln('Input not applicable. Please re-enter.'), nl, main_menu)
    ), nl,

    check_ans(Ans).


check_ans(1) :-
    read_rules,
    main_menu.

check_ans(2) :-
    start_game.

check_ans(3) :-
    writeln('Have a lovely day ^_^'),
    halt.

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
    writeln('Starting up the game… make sure you have all 4 people present and number yourselves 1-4'), nl, nl,
    % TODO:
        % player name input
        % game state initialization: deal cards
    % TEMP
    gameplay_loop(state(player([], 'a'), team(player([], 'b'), player([], 'c'), player([], 'd')), [], info(8, 0, [], []))).

gameplay_loop(state(Player, team(P2, P3, P4), Deck, Info)) :- 
    confirm_player_ready(Player),
    print_info(state(Player, Team, Deck, Info)).
%    poll_for_options.
    gameplay_loop(state(P2, team(P3, P4, Player), Deck, Info)).

confirm_player_ready(player(Hand, Name)) :-
    write('Hey, make sure only '), write(Name), write('is looking at the screen.'), nl,
    writeln('Type "okay." when you are ready!'),
    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Invalid input, please re-enter.'), nl, main_menu)
    ), nl,

    (Ans = 'okay' -> 
         writeln('Sounds good, champ!'), nl;
         writeln('Try again!'),
         confirm_player_ready(Player)).

% TODO: add played + discarded cards
% print_info(state(player([card(green,3,(false,false)),card(blue,2,(true,false)),card(red,3,(false,true)),card(white,1,(true,true))],'baba'),team(player([card(red,2,(false,true)),card(blue,4,(true,false)),card(yellow,4,(true,true)),card(white,1,(true,true))],'keke'),player([card(yellow,2,(false,true)),card(green,3,(true,false)),card(yellow,1,(true,true)),card(green,1,(true,true))],'jiji'),player([card(white,2,(false,true)),card(red,5,(true,false)),card(white,3,(true,true)),card(white,3,(true,true))],'fofo')),[],info(7,1,[],[]))).
print_info(state(player(H1, P1), team(player(H2, P2), player(H3, P3), player(H4, P4)), Deck, info(Tokens, Miss, Played, Discard))) :-
    write(P2), write('''s hand: ['), print_hand(H2), write(']'), nl,
    write(P3), write('''s hand: ['), print_hand(H3), write(']'), nl,
    write(P4), write('''s hand: ['), print_hand(H4), write(']'), nl,
    write('Your hand: ['), print_hidden_hand(H1), write(']'), nl, nl,
    format('~d information tokens, ~d misses~n', [Tokens, Miss]),
    writeln('Played: TODO'), writeln('Discarded: TODO').

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