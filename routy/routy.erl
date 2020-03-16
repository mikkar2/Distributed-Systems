-module(routy).
-export([start/1, stop/1, router/6]).


%each router will have:
%a symbolic name such as london
%a counter
%a history of received messages
%a set of interfaces
%a routing table
%a map of the network


%start(Reg, Name) ->
   % register(Reg, spawn(fun() -> init(Name) end)).


start(Name) ->
        register(Name, spawn(fun() -> init(Name) end)).


stop(Node) ->
    Node ! stop,
    unregister(Node).


init(Name) ->
    Intf = intf:new(), %empty list
    Map = map:new(), %empty list
    Table = dijkstra:table(Name, Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).


%The algorithm of a links-state protocol is as follows:
%determine which nodes that you are connected to
%tell all neighbors in a link-state message
%if you receive a link-state message that you have not seen before pass it along to your neighbors

%ligger och lyssnar, anropar sig sjalv efter att ha mottagit ett meddelande
router(Name, N, Hist, Intf, Table, Map) ->
        receive
        %intf ar en lista med links till noden, 
        {add, Node, Pid} ->
            io:format("ADD \n"),
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
             Intf1 = intf:remove(Node, Intf),
             router(Name, N, Hist, Intf1, Table, Map);

        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        %intf:broadcast skickar detta meddelande
        %links ar de noder som en stad ar connectade till {sthlm, [visby, gbg]} .... links = visby, gbg 
        {links, Node, R, Links} ->
                %om meddelandet inte har setts forut
                %skicka vidare annars gor ingenting
                case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                intf:broadcast({links, Node, R, Links}, Intf),
                Map1 = map:update(Node, Links, Map),
                router(Name, N, Hist1, Intf, Table, Map1);
                old -> %om meddelandet ar gammalt.... gor inget
                router(Name, N, Hist, Intf, Table, Map)
                end;

        %Intf ar en lista med de noder man gjort "add" pa
        %Intf innehaller {Namn, ref ,pid}
        %update... uppdaterar routing table, alltsa till vilken
        %gateway man ska skicka sitt meddelande sa att det kommer fram
        update ->
                Table1 = dijkstra:table(Name ,intf:list(Intf), Map),
                router(Name, N, Hist, Intf, Table1, Map);

        broadcast ->
                %intf:list(Intf) ... ar en lista pa Namnen av de noder man ar linkad till
                Message = {links, Name, N, intf:list(Intf)},
                intf:broadcast(Message, Intf),
                router(Name, N+1, Hist, Intf, Table, Map);

        %om To matchar med name sa ar meddelandet till mig
        {route, Name, From, Message} ->
                io:format("~w: received message ~w from ~w   ~n", [Name, Message , From]),
                router(Name, N, Hist, Intf, Table, Map);
        
        {route, To, From, Message} ->
                io:format("~w: routing message (~w)", [Name, Message]),
            %kollar vilken gateway
            case dijkstra:route(To, Table) of
                {ok, Gateway} ->

            %kollar upp piden till den gatewayen   
            case intf:lookup(Gateway, Intf) of
                {ok, Pid} ->
                %skickar vidare meddelandet.
                Pid ! {route, To, From, Message};
                notfound ->
                ok
                end;
                notfound ->
                ok
                end,
            router(Name, N, Hist, Intf, Table, Map);
            
        {send, To, Message} ->
                io:format("SEND \n"),
                %Name = namnet pa den nod som routrar vidare
                self() ! {route, To, Name, Message},
                router(Name, N, Hist, Intf, Table, Map);
      
        {status, From} ->
        io:format("STATUS \n"),
        From ! {stats, {Name, N, Hist, Intf, Table, Map}},
        %io:format("STATUS MAP: ~w" , Map),
        router(Name, N, Hist, Intf, Table, Map);

        {stats, {Name, N, Hist, Intf, Table, Map}} -> 
                io:format("STATS \n"),
                io:format("Name: ~p \n", [Name]),
                io:format("Intf: ~p \n", [Intf]),
                io:format("Table: ~p \n", [Table]),
                io:format("Map: ~p \n", [Map]),
                router(Name, N, Hist, Intf, Table, Map);      
        
        stop ->
                ok
            end.