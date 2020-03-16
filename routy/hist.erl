-module(hist). 
-export([new/1 , update/3]).


new(Name) -> 
    [{Name, 0}].

%If we know that we have seen message 15 from london
%then we know that messages from london
%with a lower number are old and
%can be thrown away. 
update(Node, N , History) ->
    case lists:keyfind(Node , 1 , History) of
        { _ , C} -> 
            if N > C -> 
                New_history = lists:keydelete(Node , 1 , History),
                {new , [{Node, N} | New_history]};
                true -> old
end;
false ->
  {new, [{Node, 0} | History]}
end.