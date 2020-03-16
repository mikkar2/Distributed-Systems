-module(node3).
-compile(export_all).

-define(Timeout, 10000).
-define(Stabilize, 10000).

start(Id) ->
    start(Id, nil).


start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Store = storage:create(),
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    io:format("New Key: ~p" , [self()]),
    node(Id, Predecessor, Successor, Store, nil).

%om det inte finns nagra andra i ringen sa ar man
%ens egna Successor
connect(Id, nil) ->
    Successor = {Id, nil, self()},
    {ok, Successor};

%peer ar en pid   
connect(Id, Peer) ->
    Qref = make_ref(), %make_ref() return a unique reference
    Peer ! {key, Qref, self()},

    receive
        {Qref, Skey} ->
        %tar emot Id pa successor, satter monitor pa varan sucessor
        Successor = {Skey, monitor(Peer), Peer},
        {ok, Successor}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.


node(Id, Predecessor, Successor, Store, Next) ->
    %io:format("~p <-> ~p <-> ~p \n", [Successor, Id, Predecessor]),
    receive

        %a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);

        %a new node informs us of its existence
        {notify, New} ->
            {Pred, New_store} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, New_store, Next);

        %a predecessor needs to know our predecessor
        {request, Peer} ->
                io:format("Req - Nxt: ~p Id: ~p \n" ,[Next, Id]),
            request(Peer, Predecessor,Next),
            node(Id, Predecessor, Successor, Store, Next);

        %our successor informs us about its predecessore
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            io:format("Nxt: ~p  \n" ,[Nxt]),
            node(Id, Predecessor, Succ, Store, Nxt);

        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);


        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);



            {'DOWN', Ref, process, _, _} ->
                    io:format("Id:  ~p \n, Pred: ~p \n, Succ: ~p \n, Next: ~p \n" , [Id, Predecessor, Successor, Next]),
                    {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
                    node(Id, Pred, Succ, Store, Nxt);

%startar ett probemeddelande som skickas ett varv
%i ringen
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store,Next);

%om jag får mitt egna Id, da har det gatt 
%ett varv. Skriv da ut tiden det tog att 
%ga ett varv, samt vilka noder som finns i cirkeln
        {probe, Id, Nodes, Time} ->
            remove_probe(Time, Nodes),
            node(Id, Predecessor, Successor, Store,Next);

        kill ->
            ok;

        status ->
                io:format("Keys: ~p \n" , [Store]),
                io:format("Id: ~p \n" , [Id]);

%lagger till sig sjalv i listan och skickar vidare.
        {probe, Ref, Nodes, Time} ->
            forward_probe(Ref, Time, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next)
        

        end.

%Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
%{Pkey, _} key to our Predecessor
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
        case key:between(Key, Pkey , Id) of
%om meddelandet ar till oss sa ar det vi som ska spara det
            true ->
                Client ! {Qref, ok},
                storage:add(Key,Value,Store);
            false ->
%om det ar false sa skickar vi det vidare till varan successor
                Spid ! {add, Key, Value, Qref, Client},
%och retunera den gamla store
                Store
        end.


lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
        case key:between(Key, Pkey , Id) of
            true ->
                Result = storage:lookup(Key, Store),
                Client ! {Qref, Result};
            false ->
                {_, Spid} = Successor,
                Spid ! {lookup, Key, Qref, Client}
        
        end.

%startar ett probemeddelande. 
create_probe(Id, Successor) ->
        {_ ,Ref, SPID} = Successor,
        SPID ! {probe, Id, [{Id,Ref}], erlang:system_time(micro_seconds)}.
    
forward_probe(Ref, Time, Nodes, Id, Successor) ->
    {_,RR,SPID}=Successor,
    %lägger till Id i Listan av noder som finns i listan
    SPID ! {probe, Ref,[{Id, RR}|Nodes],Time}.

%gatt ett varv, dags att skriva ut resultaten
remove_probe(Time, Nodes) ->
    io:format("Time: ~w \n", [Time]),
    io:format("Nodes: ~w \n", [Nodes]).

%The Pred argument is ours successors current predecessor.
stabilize(Pred , Next, Id, Successor) ->
    {Skey, Sref ,Spid} = Successor,
    io:format("Next: ~p  Id: ~p\n" , [Next, Id]),

    case Pred of
        %informera om att vi finns som noden innan
        nil ->
            Spid ! {notify, {Id , self()}},
            {Successor, Next};
        {Id, _} ->
            {Successor, Next};
        %den pekar pa sig sjalv, sag att vi finns
        {Skey, _} ->
            Spid ! {notify, {Id , self()}},
            {Successor, Next};
%If it’s pointing to another node we need to be careful
%If the key of the predecessor of our successor (Xkey)
%is between us and our successor we should of course adopt this node as our
%successor and run stabilization again
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
        %om den ligger emellan oss och den noden
        %ska vi meddela X att vi ligger bakom
        %och var nya successor blir den nodes pred.
                true ->
                    Xpid ! {request, self()},
                    drop(Sref),
                    Nref = monitor(Xpid),
                    io:format("true: ~p  Id: ~p\n" , [Nref, Id]),
                   {{Xkey, Nref, Xpid} , Successor};
    
        %om inte, meddela att vi ligger bakom
                false ->
                    Spid ! {notify, {Id , self()}},
                    {Successor, Next}
                end
end.


schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_,_, Spid}) ->
    Spid ! {request, self()}.


request(Peer, Predecessor ,Next) ->
    case Predecessor of
        nil ->
            Peer ! {status, Predecessor, Next};
    
        {Pkey,_, Ppid} ->
            Peer ! {status, {Pkey, Ppid} , Next}
end.

%Being notified of a node is a way for a node to make a friendly proposal that
%it might be our proper predecessor. We can not take their word for it, so we
%have to do our own investigation.
%{Nkey, Npid} = den som foreslar att han ar pre
% Id, ar vart nummer i ringen
%Predecessor ar varan nuvarande pred
notify({Nkey, Npid}, Id, Predecessor) ->
    New_pred = {Nkey, Npid},
    case Predecessor of
%om jag inte har nagon pre.. satt ny
        nil ->
            New_pred;
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    New_pred;
                false ->
                    Predecessor
            end
        end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
%om jag inte har nagon pre.. satt ny
        nil ->
%Keep = de nycklarna i min store jag ska behalla
             Keep = handover(Id, Store, Nkey, Npid),
             NewPred = {Nkey, monitor(Npid), Npid},
             {NewPred , Keep};
%om jag har en predecessor
        {Pkey,Ref, _} ->   
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    drop(Ref),
                    Ppref = monitor(Npid),
                    NewPred = {Nkey, Ppref, Npid},
                    {NewPred , Keep};
                false ->
                    {Predecessor, Store}
                end
    end.

handover(Id, Store, Nkey, Npid) ->
        {Rest, Keep} = storage:split(Id, Nkey, Store),
        Npid ! {handover, Rest},
        Keep.


%storage:split(Id, Nkey, Store):
%return a tuple {Updated, Rest} where the
%updated store only contains the key value pairs requested
%and the rest are found in a list of key-value pairs


monitor(Pid) ->
       % io:format("Moin ~p \n", [Pid]),
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Pid) ->
   % io:format("drop ~p \n", [Pid]),
    erlang:demonitor(Pid, [flush]).

%ref ar ref till processen som dott
%{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
down(Ref, {_, Ref, _}, Successor, Next) ->
    %om varan pred dog, satt pred till nil
    {nil, Successor, Next};

%detta om succ dor
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
Nref = monitor(Npid),
{Predecessor, {Nkey, Nref, Npid}, nil}.