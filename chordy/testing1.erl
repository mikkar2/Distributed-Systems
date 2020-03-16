-module(testing1).
-compile(export_all).

s() ->
    N1 = node1:start(1),
    timer:sleep(1000).
   % N2 = node1:start(2, n1).