-module(vect).
-export([zero/0 ,inc/2 , merge/2 , leq/2, clock/1 , update/3, safe/2]).

zero() -> 
    [].


inc(Name, Time) ->
%kolla om den finns i listan och addera 1
case lists:keyfind(Name, 1, Time) of
    {Name , Current_time} ->
    lists:keyreplace(Name, 1, Time, {Name, Current_time + 1});
false ->
%om den inte finns i listan, lagg till den.
[{Name, 0}|Time]
end.

get_max(Ti, Tj) ->
    if Ti > Tj -> Ti;
    Ti =< Tj -> Tj
end.

%merge gar igenom tva vektorer och mergar varderna
merge([], Time) ->
Time;

merge([{Name, Ti}|Rest], Time) ->
%om Noden finns i listan.. uppdatera den
case lists:keyfind(Name, 1, Time) of
{Name, Tj} ->
Max = get_max(Ti, Tj),
[{Name , Max} |merge(Rest, lists:keydelete(Name, 1, Time))];
%om den inte finns i listan... lagg till den
false ->
[{Name, Ti} |merge(Rest, Time)]
end.


%We only have to remember that a vector time stamp is less than or equal to
%another timestamp if each of its entries are less than or equal to the entries
%of the other timestamp. If the other time stamp does not have an entry for
%a given process that means that it implicitly has a zero entry
leq([], _) ->
true;

leq([{Name, Ti}|Rest],Time) ->
case lists:keyfind(Name, 1, Time) of
{Name, Tj} ->
if Ti =< Tj ->
leq(Rest , Time);
true ->
false
end;
false ->
false
end.


create_clocks([] , Clocklist) -> Clocklist;
create_clocks([H | T] , Clocklist) ->
    create_clocks(T , [{H , zero()} | Clocklist]).


clock(_) ->
        [].


update(From, Time, Clock) ->
        T1 = lists:keyfind(From, 1, Time),
        case lists:keyfind(From, 1, Clock) of
        {From, _} ->
        lists:keyreplace(From, 1, Clock, T1);
        false ->
        [T1| Clock]
        end.

safe(Time, Clock) ->
        leq(Time, Clock).