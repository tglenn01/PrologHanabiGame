library(statistics).
:- use_module(library(tty.pl)).

test_test() :-
    writeln('Point A'),
    cls1,
    writeln('Point B'),
    cls2,
    writeln('Point C'),
    cls3,
    writeln('Point D').

cls1 :- write('\33\[2J').
cls2 :- write('\e[2J').s
cls3 :- write('\e[H\e[2J').

init_time() :-
    thread_create(probe_time(true,0), ThreadId),
    writeln('Point A'),
    sleep(65),
    writeln('Point B'),
    catch(
        thread_signal(ThreadId, exit),
        error(_, _Context),
        (writeln('Finished After The Time Limit.'),
        false)
    ),
    writeln('Point C').
    


turn_timer(IsFirstCall, InitialTime) :- 
    sleep(1),
    statistics(real_time, [RealTime,_]),
    (IsFirstCall -> 
        turn_timer(false,RealTime);
        TimeDiff is RealTime - InitialTime,
        (TimeDiff = 30 ->
            writeln('thirty seconds remaining'),
            turn_timer(false,InitialTime);
            (TimeDiff = 50 ->
                writeln('ten seconds remaining'),
                turn_timer(false,InitialTime);
                (TimeDiff >= 60 ->
                    writeln('times up!');
                    turn_timer(false,InitialTime))))).