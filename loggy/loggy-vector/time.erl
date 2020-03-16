-module(time).
-export([zero/0 ,inc/2 , merge/2 , leq/2, clock/1 , update/3, safe/2]).

%istallet for att en worker endast har sin egna tid har den en vektor med allas tid
%logger har en vektor precis som innan

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
%time:merge(Time, Logic_clock),
%Time = en vektor [{paul,2},{john,2},{ringo,1}] som ska mergas med nodens egna time som ocksa ar en vektor 
merge([{Name, Ti}|Rest], Time) ->
%om Noden finns i listan.. uppdatera den
case lists:keyfind(Name, 1, Time) of
{Name, Tj} ->
Max = get_max(Ti, Tj),
[{Name , Max} |merge(Rest, lists:keydelete(Name, 1, Time))];
%om den inte finns... lagg till den
false ->
[{Name, Ti} |merge(Rest, Time)]
end.



leq([], _) ->
true;
%Ti tiden i medelandetvectorn
%om tiden i logklockan ar storre an tiden i meddelandet... ga vidare.
%gar igenom Nod for Nod och kollar att alla ar lagre, inte bara for det lagsta meddelandet
leq([{Name, Ti}|Rest],Clock) ->
case lists:keyfind(Name, 1, Clock) of
    {Name, Tj} ->
        if Ti =< Tj -> leq(Rest , Clock);
        Ti > Tj -> false
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

%Time e vectorn fran meddelandet
%Clock e vectorn hos loggerz
safe(Time, Clock) ->
        leq(Time, Clock).