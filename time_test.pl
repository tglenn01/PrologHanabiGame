library(statistics).


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
    


probe_time(IsFirstCall, InitialTime) :- 
    sleep(1),
    statistics(real_time, [RealTime,_]),
    (IsFirstCall -> 
        probe_time(false,RealTime);
        TimeDiff is RealTime - InitialTime,
        (TimeDiff = 30 ->
            writeln('thirty seconds remaining'),
            probe_time(false,InitialTime);
            (TimeDiff = 50 ->
                writeln('ten seconds remaining'),
                probe_time(false,InitialTime);
                (TimeDiff >= 60 ->
                    writeln('times up!');
                    probe_time(false,InitialTime))))).