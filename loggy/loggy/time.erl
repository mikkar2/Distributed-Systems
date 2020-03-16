-module(time).
-export([zero/0 ,inc/2 , merge/2 , leq/2, clock/1 , update/3, safe/2]).

%return an initial Lamport value (could it be 0)
zero() -> 
    0.

% return the time T incremented by one 
%(you will probably ignore the Name but we will use it later)
inc(Name, T) ->
    T + 1.


%merge(Ti, Tj) : merge the two Lamport time stamps 
%(i.e. take the maximum value)
merge(Ti, Tj) ->
    if Ti > Tj -> Ti;
    Tj >= Ti -> Tj
end.

%om den lagsta tiden bland noderna ar hogre an meddelandets
%betyder det att noderna har tagit emot events som hander
%efter tidpunkten for meddelandet och det ar sakert att
%printa meddelandet.
leq(Message_Time, Node_Time) ->
    if Message_Time =< Node_Time -> true;
    Message_Time > Node_Time -> false
end.


%initierar en klocka for varje nod. 
create_clocks([] , Clocklist) -> Clocklist;
create_clocks([H | T] , Clocklist) ->
    create_clocks(T , [{H , zero()} | Clocklist]).


%return a clock that can keep track of the nodes
clock(Nodes) ->
    List_of_clocks = create_clocks(Nodes , []).

%uppdaterar tiden for en nod i klockan
update(Node, Time, Clock) ->
    Updated_clock = lists:keyreplace(Node , 1 , Clock , {Node , Time}),
    Sorted_Updated_clock = lists:keysort(2, Updated_clock).

%is it safe to log an event that happened at a given
%time, true or false
%om vi har sparade meddelanden som har ett tidigare time-stamp
%sa ar det saker att printa ut det meddelandet. 
%safe(meddelandets tid, lagsta tiden bland noderna)
safe(Message_Time, Node_Time) ->
    leq(Message_Time , Node_Time).