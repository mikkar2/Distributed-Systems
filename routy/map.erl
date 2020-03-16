-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).
%-export([new/0, update/3, main/0, reachable/2]).



%keyfind/3

%keyfind(Key, N, TupleList) -> Tuple | false
%Types
%Key = term()
%N = integer() >= 1
%1..tuple_size(Tuple)
%TupleList = [Tuple]
%Tuple = tuple()
%Searches the list of tuples TupleList 
%for a tuple whose Nth element compares equal to Key. 
%Returns Tuple if such a tuple is found, otherwise false.

%keydelete/2

%keydelete(Key, N, TupleList1) -> TupleList2
%Types
%Key = term()
%N = integer() >= 1
%1..tuple_size(Tuple)
%TupleList1 = TupleList2 = [Tuple]
%Tuple = tuple()
%Returns a copy of TupleList1 where the first occurrence 
%of a tuple whose Nth element compares equal to Key is deleted, 
%if there is such a tuple.

%map/2

%map(Fun, List1) -> List2
%Types
%Fun = fun((A) -> B)
%List1 = [A]
%List2 = [B]
%A = B = term()
%Takes a function from As to Bs, 
%and a list of As and produces a list 
%of Bs by applying the function to every element in the list. 
%This function is used to obtain the return values. 
%The evaluation order depends on the implementation.


main() ->
    Map = [{tokyo , [london, paris]} , {stockholm, [barcelona, helsinki]}],
    Gap = map:update(stockholm,c [kiruna, skovde], Map),
    map:reachable(stockholm, Gap),
    map:reachable(stockholm, Map).
    


%creates an empty list
new() -> 
    [].

%New_map innehaller map utan Node
%lagger sedan in Noden langst fram
%exempel pa ett anrop:
%map:update(berlin, [london, paris], []).
%[{berlin,[london,paris]}]
update(Node , Links, Map) ->
    New_map = lists:keydelete(Node , 1 , Map),
    [{Node, Links} | New_map].

%returns the list of nodes directly reachable
%from Node.
reachable(Node, Map) ->
    case lists:keyfind(Node, 1 , Map) of
        {Ret_node , List_of_reachable_nodes} ->
                List_of_reachable_nodes;
        false -> 
            []
end.

%returns a list of all nodes in the map, also the ones
%without outgoing links 
all_nodes([]) -> [];
all_nodes([{Node ,Links}|T]) ->
    [Node] ++ Links ++ all_nodes(T).
