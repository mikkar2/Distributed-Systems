-module(node1).
-compile(export_all).

-define(Timeout, 10000).
-define(Stabilize, 100).


%stabilize
%fragar min sucessor om hans predeccesor 
%om han har en får jag den som svar annars nil
%jag går till stabilize med den nya pred stabilize(Pred, Id, Successor)
%da kollar jag om den ligger emellan efter osv.
%finns nagra fall
%

start(Id) ->
    start(Id, nil).


start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

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


node(Id, Predecessor, Successor) ->
    %io:format("~p <-> ~p <-> ~p \n", [Successor, Id, Predecessor]),
    receive
        %a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);

        %a new node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);

        %a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);

        %our successor informs us about its predecessore
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);

        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);

%startar ett probemeddelande som skickas ett varv
%i ringen
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);

%om jag får mitt egna Id, da har det gatt 
%ett varv. Skriv da ut tiden det tog att 
%ga ett varv, samt vilka noder som finns i cirkeln
        {probe, Id, Nodes, Time} ->
            remove_probe(Time, Nodes),
            node(Id, Predecessor, Successor);

%lagger till sig sjalv i listan och skickar vidare.
        {probe, Ref, Nodes, Time} ->
            forward_probe(Ref, Time, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
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
        %det ar jag
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

%stabilize
%fragar min sucessor om hans predeccesor 
%om han har en far jag den som svar annars nil
%om det ar jag som ar min succ pred... gor ingenting
%annars:
%jag gar till stabilize med den nya pred stabilize(Pred, Id, Successor)
%da kollar jag om den ligger emellan efter osv.
%finns nagra fall:
%om den inte har nagon pred... skickar jag notify till var sucessor (talar om att vi finns) att vi borde vara deras pred
%om det ar jag skicka successor
%pekar den pa sig sjalv... sag att vi finns
%pekar den pa nagon annan nod kollar vi hur den ligger:
%ligger den emellan varan successors predecessor emellan mig och successorn blir den saklart varan sucessor ist. da fragar vi den om dess predecessor och allt borjar om
%ligger den inte emellan sa ar varan sucessor suceceesor, da sager vi till varsuccessor att vi ar deras predeccessor.



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
        nil ->
            New_pred;
        {Pkey, _} ->
%kollar om den nya nyckeln ligger mellan oss och den gamla pred.
            case key:between(Nkey, Pkey, Id) of
                true ->
                    New_pred;
                false ->
                    Predecessor
            end
        end.
