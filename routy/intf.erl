-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).


new () ->
    [].
%Name = namn pa noden, 
%ref = erlang monitor
%pid = pid till noden
%intf resten av listan med links
add(Name , Ref , Pid , Intf) -> 
    [{Name , Ref , Pid } | Intf].

remove(Name , Intf) ->
    lists:keydelete(Name, 1 , Intf).


lookup(Name , Intf) ->
    case lists:keyfind(Name , 1 , Intf) of 
        {_, _ , Pid} -> {ok , Pid};

    false -> notfound
end.

ref(Name , Intf) ->
    case lists:keyfind(Name , 1 , Intf) of 
        {_ , Ref , _} -> {ok , Ref};

    false -> notfound
end.


name(Ref , Intf) ->
    case lists:keyfind(Ref , 2 , Intf) of 
        {Name , _ , _} -> {ok , Name};

    false -> notfound
end.

%retunerar en lista med alla namnen pa alla Noder man har
list([]) -> [];
list([{Name , _ , _} | T]) ->
    [Name | list(T)].


broadcast(_ , []) ->
    ok;
%Message = {links, Node, R, Links} 
%skickar messege till alla links
broadcast(Message, [{_ , _ , Pid} | T]) ->
    Pid ! Message, 
    broadcast(Message , T).