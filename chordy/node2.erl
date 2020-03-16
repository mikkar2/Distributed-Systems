-module(node2).
-compile(export_all).

-define(Timeout, 10000).
-define(Stabilize, 100).

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
    node(Id, Predecessor, Successor, Store).

%om det inte finns nagra andra i ringen sa ar man
%ens egna Successor
connect(Id, nil) ->
    Successor = {Id, self()},
    {ok, Successor};
    
connect(Id, Peer) ->
    Qref = make_ref(), %make_ref() return a unique reference
    Peer ! {key, Qref, self()},

    receive
        {Qref, Skey} ->
        Successor = {Skey, Peer},
        {ok, Successor}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.


node(Id, Predecessor, Successor, Store) ->
    %io:format("~p <-> ~p <-> ~p \n", [Successor, Id, Predecessor]),
    receive
        %a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);

        %a new node informs us of its existence
        {notify, New} ->
            {Pred, New_store} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, New_store);

        %a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);

        %our successor informs us about its predecessore
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);

        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);


        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);

        status ->
            io:format("Keys: ~p \n" , [Store]),
            io:format("Id: ~p \n" , [Id]);

%startar ett probemeddelande som skickas ett varv
%i ringen
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);

%om jag får mitt egna Id, da har det gatt 
%ett varv. Skriv da ut tiden det tog att 
%ga ett varv, samt vilka noder som finns i cirkeln
        {probe, Id, Nodes, Time} ->
            remove_probe(Time, Nodes),
            node(Id, Predecessor, Successor, Store);

%lagger till sig sjalv i listan och skickar vidare.
        {probe, Ref, Nodes, Time} ->
            forward_probe(Ref, Time, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store)
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
        {_ , SPID} = Successor,
        SPID ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.
    
forward_probe(Ref, Time, Nodes, Id, Successor) ->
    {_,SPID}=Successor,
    %lägger till Id i Listan av noder som finns i listan
    SPID ! {probe, Ref,[Id|Nodes],Time}.

%gatt ett varv, dags att skriva ut resultaten
remove_probe(Time, Nodes) ->
    io:format("Time: ~w \n", [Time]),
    io:format("Nodes: ~w \n", [Nodes]).

%The Pred argument is ours successors current predecessor.
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,

    case Pred of
        %informera om att vi finns som noden innan
        nil ->
            Spid ! {notify, {Id , self()}},
            Successor;
        {Id, _} ->
            Successor;
        %den pekar pa sig sjalv, sag att vi finns
        {Skey, _} ->
            Spid ! {notify, {Id , self()}},
            Successor;
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
                    Pred;
    
        %om inte, meddela att vi ligger bakom
                false ->
                    Spid ! {notify, {Id , self()}},
                    Successor
                end
end.


schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.


request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
    
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
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
             NewPred = {Nkey, Npid},
             {NewPred , Keep};
%om jag har en predecessor
        {Pkey, _} ->   
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    NewPred = {Nkey, Npid},
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