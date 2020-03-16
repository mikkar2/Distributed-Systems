-module(loggerz).
-export([start/1, stop/1]).


%Nar en worker skickar eller tar emot ett meddelande
%sa skickar den det till logger
%antingen:
%* {log, Name, Incremented_logic_clock, {received, Msg}}
%*Log ! {log, Name, Time, {sending, Message}}
%det ar loggers uppgift att halla reda pa vilken ordning
%meddelanden har skickat och skriva ut som i korrekt ordning

start(Nodes) ->
        spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
        Logger ! stop.

init(Nodes) ->
        Que = [],
        loop(Que, time:clock(Nodes)).

%The logger should have a clock that keeps track of the timestamps of the
%last messages seen from each of the workers. It should also have a hold-back
%queue where it keeps log messages that are still no safe to print. When a
%new log message arrives it should update the clock, add the message to the
%hold-back queue and then go through the queue to find messages that are
%now safe to print.

%clock ar en lista med de workers som finns 
%den initieras till:
%[{worker1 , 0} , {worker2, 0} ... ]
%nar ett nytt meddelande fran en worker uppdateras klockan
%[{worker1 , 1} , {worker2, 0}] osv,
loop(Que, Clock) ->
%Msg ar antinge {Sending , Message} lr {Receiving, Message}
        receive
        {log, From, Time, Msg} ->
                %uppdaterar klockan. 
                Updated_clock = time:update(From , Time , Clock),
                %lagger meddelandet pa hold-back-que
                Updated_que = add_messege_to_que(From , Time , Msg , Que),
                %printar de meddelande som ar "safe" att printa
                New_que=iterate(Updated_que, Updated_clock , []),
                loop(New_que, Updated_clock);
        stop ->
                io:format("STOP \n"),
                ok
                end.
                log(From, Time, Msg) ->
                io:format("log: ~w ~w ~p~n", [Time, From, Msg]).


%lagger till meddelandet till kon
%alla meddelanden som kommer in laggs har. 
add_messege_to_que(From , Time , Msg , Que) ->
        [{From , Time , Msg} | Que].

iterate([] , _ , New_que) ->
        New_que;
%tar emot en  SORTERAD lista med noder och deras respektive klocka
%samt en SORTERAD lista med meddelanden och vilket klockslag dom kom
%Clock = [{george,1},{paul,5},{john,5},{ringo,5}]
%Message_que =  [{paul,[{paul,5},{john,6},{ringo,4},{george,0}],{received,{hello,27}}},{ringo,[{ringo,5},{john,5},{paul,4},{george,2}],{received,{hello,49}}}]
iterate(Message_que , Clock , New_que) ->
        [{Node , Vector , Msg} | T] = Message_que, 

        %istallet for att endast skicka med den lagsta tidpunkten som innan skickar jag med en hel vector 
        %o jamfor mot varje element i vectorn vilket ar korkad? kan ju bara sorterara och kolla mot den minsta som 
        %jag gjorde innan.
        case time:safe(Vector , Clock ) of 
            
                true ->
                io:format("log: ~w ~w ~p~n", [Vector, Node, Msg]),
                iterate(T, Clock, New_que);
                false ->
                iterate(T, Clock, [{Node , Vector , Msg} | New_que])
        end.

                















                

