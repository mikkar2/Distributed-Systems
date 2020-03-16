-module(testing).
-compile(export_all).


testing() ->
W1 = test:first(1, gms4, 1000),
timer:sleep(3000),
test:add(2, gms4, W1, 1000),
timer:sleep(3000),
test:add(3, gms4, W1, 1000),
test:add(5, gms4, W1, 1000),
timer:sleep(3000),
test:add(6, gms4, W1, 1000).