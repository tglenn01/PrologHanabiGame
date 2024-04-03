%
%
% Frontend insipired by https://github.com/cjyu98/CPSC-312-Book-Recommender/blob/main/main.pl
%
%
%


% start
% starts the application by loading the knowledge base and starting the main menu
start:-
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
    writeln('TLDR: Play with 4 friends and work together to play your hands in order'),
    writeln('You each are given a hand of 5 cards, where you cant see your own hand but can see your teammates'),
    writeln('Cards have a color [red, blue, green, yellow] and a number [1-5]'),
    writeln('The gamefield starts with a [Red 0], [Green 0], [Yellow 0], and a [Blue 0]'),
    writeln('Your goal as a team is to count each colour to 5 going in order'), nl,
    writeln('During your turn you have 3 options'),
    writeln('[1] Use one of your information tokens to tell your teammates information about their hand'),
    writeln('[2] Play a card onto the field (blind!) and guess that you are making a valid move'),
    writeln('[3] Discard one of you cards'), nl, nl.

start_game :-
    writeln('Starting up the game make sure you have all 4 people present and number yourselves 1-4'), nl, nl,
%    deal_cards,
    gameplay_loop(0).


gameplay_loop(Player) :- 
    confirm_player_ready(Player).
%    print_info,
%    poll_for_options.

confirm_player_ready(Player) :-
    format('Hey make sure its only [Player ~d] looking at the screen', [Player]), nl,
    writeln('type "okay." when you are ready to go!'),
    catch(
        read(Ans),
        error(syntax_error(_), _),
        (writeln('Input not applicable. Please re-enter.'), nl, main_menu)
    ), nl,

    (Ans = 'okay' -> 
         writeln('sounds good champ!');
         writeln('Try again loser'),
         confirm_player_ready(Player)).

         

