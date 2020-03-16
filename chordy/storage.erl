-module(storage).
-compile(export_all).


%create a new store
create() ->
    [].

%add a key value pair, return the updated store
add(Key, Value , Store) ->
    [{Key, Value} | Store].

%return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    case lists:keyfind(Key, 1, Store) of
        {K, Value} -> {K, Value};
        false ->
            false
    end.



%return a tuple {Updated, Rest} where the
%updated store only contains the key value pairs requested
%and the rest are found in a list of key-value pairs
split(From, To, Store) ->
    {Uptaded , Rest} = splitter(From, To, Store , [], []).
 

splitter(From, To, [] , Updated, Rest) ->
        {Updated, Rest};
        
splitter(From, To, [{Key, Value }| T] , Updated, Rest) ->
    case key:between(Key, From, To) of
        true -> splitter(From, To, T, [{Key,Value} | Updated], Rest);
        false -> splitter(From, To, T, Updated, [{Key,Value} | Rest])
end.


%add a list of key-value pairs to a store
merge(Entries, Store) ->
    Entries ++ Store.