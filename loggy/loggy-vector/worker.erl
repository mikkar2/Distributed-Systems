-module(worker).
-export([start/5, stop/1, peers/2]).

%Jitter, or network jitter, is the variance in time delay in milliseconds (ms)
%between data packets over a network. It is a disruption in the normal sequence of sending data packets.

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.
    

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
    %Peers ar en lista med andra workers.. alltsa andra processer
    {peers, Peers} ->
        loop(Name, Log, Peers, Sleep, Jitter , time:zero());
    stop ->
            ok
    end.


peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.



%The worker process is quite simple. The process will wait for either a
%message from one of its peers or after a random sleep time select a peer
%process that is sent a message. The worker does not know about time so
%we simply create a dummy value, na, in order to have something to pass
%to the logger. The messages could of course contain anything but here we
%include a hopefully unique random value so that we can track the sending
%and receiving of a message.

loop(Name, Log, Peers, Sleep, Jitter, Logic_clock)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            %Time = en vektor [{paul,2},{john,2},{ringo,1}]
        %Innan var time endast en tidpunkt fran den som skickade meddelandet
            %Logic_clock ar ocksa en vektor  [{paul,2},{john,2},{ringo,1}]
             Merged_logical_clocks = time:merge(Time,Logic_clock),   
             Incremented_logic_clock = time:inc(Name, Merged_logical_clocks),
             Log ! {log, Name, Incremented_logic_clock, {received, Msg}},
             loop(Name, Log, Peers, Sleep, Jitter, Incremented_logic_clock);
        stop ->
            ok;
    Error ->
        Log ! {log, Name, time, {error, Error}}
    %om worker inte har fatt nagot meddelande efter 
    %Wait = x sec, sa tar den en random worker och skickar 
    %ett meddelande.
    after Wait ->
    Selected = select(Peers),
    Time = time:inc(Name , Logic_clock),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Time, Message},
    %jitter ar att worker sleepar i y sec
    jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, Time)
        end.


%The selection of which peer to send a message to is random
select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).


%jitter introduces a slight delay between sending the message to the peer and
%informing the logger. If we don’t introduce a delay here we would hardly
%ever have messages occur out of order when running in the same virtual
%machine.
jitter(0) -> 
    ok;

jitter(Jitter) -> 
    timer:sleep(random:uniform(Jitter)).