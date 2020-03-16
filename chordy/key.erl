-module(key).
-compile(export_all).

%The function generate/0 will simply return a random number
%from 1 to 1.000.000.000 (30-bits),
generate() -> 
    Rnd = random:uniform(1000000000).


%The between/3 function will check if a Key 
%is between From and To or equal to To
%Remember that the we’re dealing with a 
%ring so it could be that From is larger than To.
%1. 3 , mellan 4 och 9
between(Key,From, To) when To==From ->
    true;

%nar to ar storre an from

between(Key,From, To) when From<To ->

    if 
%om nyckel ligger mellan to och from samt att to ar storre an from
        (Key=<To) and (From<Key) ->
            true;
        true -> 
            false
    end;

%gar over noll i cirkeln
between(Key,From, To) when To<From ->
    
    if
        (Key=<To) or (Key>From) ->
            true;
        true -> 
            false
end.






