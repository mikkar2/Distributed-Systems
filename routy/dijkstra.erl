-module(dijkstra).
-export([entry/2 , replace/4 , update/4, iterate/3, table/3, route/2]).

%Sorted = sorterad lista over hur man kan na destinationer
%ex: {berlin , 2 , london}.... berlin kan nas av 2 hopp via gateway london

%Map = en karta
%ex: [{berlin, [madrid]} , {london, [tokyo]}] .... berlin har en anslutning till madrid

%Table = routing table
%ex: [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]
% vill na berlin borde vi skicka meddelandet till madrid 


%: returns the length of the shortest path to the
%node or 0 if the node is not found.
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1 , Sorted) of
        {Destination, Hops, Gateway} -> Hops;
    false ->
        0
end.


%replaces the entry for Node
%in Sorted with a new entry having a new length N and Gateway. The
%resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    case lists:keyreplace(Node, 1 , Sorted , {Node , N , Gateway}) of 
        New_list -> lists:keysort(2 , New_list)
end.



%update the list Sorted given
%the information that Node can be reached in N hops using Gateway.
%If no entry is found then no new entry is added. Only if we have a
%better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    Nr_of_hops = entry(Node , Sorted),

if Nr_of_hops > N ->
    replace(Node , N , Gateway, Sorted)
    ; Nr_of_hops =< N ->
        Sorted 
end.


%If there are no more entries in the sorted list then we are done and the
%given routing table is complete.
iterate([] , Map , Table) ->
    Table;

%If the first entry is a dummy entry with an infinite path to a city we
%know that the rest of the sorted list is also of infinite length and the
%given routing table is complete.

iterate([{Node , inf , Gateway} | T], Map, Table) ->
    Table;

%iterate(Sorted, Map, Table) ->
iterate(Sorted, Map, Table) ->
    [{Node, Hops , Gateway} | T] = Sorted,
    %retunerar en lista med de noder som ar direkt anslutna till noden.
    case map:reachable(Node , Map) of 
        [] -> iterate(T, Map , [{Node, Gateway} | Table]);
        Reachable_nodes -> 
        %uppdatera alla direkt anslutna noder 
        New_sorted = iterate_helper_function(Reachable_nodes, Hops , Gateway, T),
        iterate(New_sorted, Map , [{Node, Gateway} | Table])
end.

    

iterate_helper_function([], _ , _ , Sorted ) ->
    Sorted;

iterate_helper_function([H | T] , Hops , Gateway, Sorted) ->
    %H kan nas med ett hopp fran gateway
    New_sorted = update(H , Hops  + 1 , Gateway , Sorted ),
    iterate_helper_function(T, Hops , Gateway , New_sorted ).


initialize_table(_ , [], Sorted) ->
    Sorted;

%satter 0 i avstand till de noder som ar direkt anslutna
%annars oandligt avstand
initialize_table(Gateways , [H | T], Sorted) ->
    %lists:member returns true if H matches some elem in Gateways
    case lists:member(H, Gateways) of
        %nar sig sjalv via 0 hops, alltsa dom vi kan skicka meddelande direkt till
        true -> initialize_table(Gateways , T , [{H , 0 , H} | Sorted]);
        false -> initialize_table(Gateways, T , [{H, inf , unknown} | Sorted])
end.

%Gateways ar intf:list(Intf).. alltsa en lista med namnen till 
%noder som ligger i anslutning
%gateway ar de noder man har i direkt anslutning
table(Name, Gateways , Map) ->
    %sorterad lista av alla noder som finns i kartan
    All_nodes = lists:usort(map:all_nodes(Map)),
    %skapar forst en sorted lista
%initialize lagger in direkt anslutna noder som (lund , 0 , lund)
%de som inte ar direkt anslutna (koping , inf , unknown)
    Sorted = initialize_table(Gateways, All_nodes, []),
    %skapar ett table av sorted
    %sorterar sorted efter antalet hop
    Table = iterate(lists:keysort(2, Sorted), Map, []).

%kollar om man har noden i sitt table, skickar till vilken
%gateway meddelandet ska skickas vidare
route(Node , Table) ->
    case lists:keyfind(Node , 1 , Table) of 
        { _ , Gateway} -> {ok , Gateway};
        false -> notfound
end.








        