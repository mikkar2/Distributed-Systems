-module(testing).
-export([start_test/0]).

%Stockholm -------------------- Lund
%   |                              |
%   |                              |
%   |                              |
%   |                              |
%Sundsvall --------------------- Koping

adr() -> 
    'one@n167-p49'.

start_test() ->
io:format("Start router Stockholm \n"),
routy:start(stockholm),
timer:sleep(500),
io:format("Start router Lund \n"),
routy:start(lund),
timer:sleep(500),
io:format("Start router Sundsvall \n"),
routy:start(sundsvall),
timer:sleep(500),
io:format("Start router Koping \n"),
routy:start(koping),
timer:sleep(500),

%creating connection between stockholm <-> lund
stockholm ! {add, lund , {lund, adr()}},
timer:sleep(500),
lund ! {add, stockholm , {stockholm, adr()}},
timer:sleep(500),
io:format("stockholm status after adding lund: \n"),
{stockholm , adr()} ! {status, {stockholm, adr()}},
timer:sleep(500),

%creating connection between lund <-> koping
lund ! {add, koping , {koping, adr()}},
timer:sleep(500),
koping ! {add, lund , {lund, adr()}},
timer:sleep(500),
io:format("lund status after adding stockholm and koping: \n"),
timer:sleep(500),
{lund, adr()} ! {status, {lund, adr()}},
timer:sleep(500),

%creating connection between koping <-> sundsvall
sundsvall ! {add, koping , {koping, adr()}},
timer:sleep(500),
koping ! {add, sundsvall , {sundsvall, adr()}},
timer:sleep(500),
io:format("koping status after adding lund and sundsvall: \n"),
timer:sleep(500),
{koping, adr()} ! {status, {koping, adr()}},


%creating connection between stockholm <-> sundsvall
sundsvall ! {add, stockholm , {stockholm, adr()}},
timer:sleep(500),
stockholm ! {add, sundsvall , {sundsvall, adr()}},
timer:sleep(500),
io:format("sundsvall status after adding koping and stockholm: \n"),
timer:sleep(500),
{koping, adr()} ! {status, {koping, adr()}},


stockholm ! broadcast,
timer:sleep(500),
io:format("stockholm status after stockholm ! broadcast: \n"),
{stockholm , adr()} ! {status, {stockholm, adr()}},
timer:sleep(500),
lund ! broadcast,
timer:sleep(500),
koping ! broadcast,
timer:sleep(500),
io:format("koping status after koping ! broadcast: \n"),
{koping, adr()} ! {status, {koping, adr()}},
timer:sleep(500),
sundsvall ! broadcast,
timer:sleep(500),



stockholm ! update,
timer:sleep(500),
io:format("stockholm status after stockholm ! update: \n"),
{stockholm , adr()} ! {status, {stockholm, adr()}},
timer:sleep(500),
lund ! update,
timer:sleep(500),
koping ! update,
timer:sleep(500),
io:format("koping status after koping ! update: \n"),
{koping, adr()} ! {status, {koping, adr()}},
timer:sleep(500),
sundsvall ! update,
timer:sleep(500), 



%sending message stockholm - sundsvall
io:format("sending message stockholm - sundsvall \n"),
stockholm ! {send, sundsvall , hej},
timer:sleep(500),
timer:sleep(500),


%removing link between stockholm and sundsvall
io:format("Stop router Sundsvall \n"),
routy:stop(sundsvall),

timer:sleep(500),
stockholm ! broadcast,
timer:sleep(500),

stockholm ! update,
timer:sleep(500),
io:format("stockholm status after stockholm ! update: \n"),
{stockholm , adr()} ! {status, {stockholm, adr()}},
timer:sleep(500),
%lund ! update,
timer:sleep(500),
koping ! update,
timer:sleep(500),
io:format("koping status after sundsvall removed: \n"),
{koping, adr()} ! {status, {koping, adr()}},
timer:sleep(500), 

timer:sleep(500),
io:format("Re-start router Sundsvall \n"),
routy:start(sundsvall),
timer:sleep(500),
%creating connection between sundsvall <-> koping
sundsvall ! {add, koping , {koping, adr()}},
timer:sleep(500),
koping ! {add, sundsvall , {sundsvall, adr()}},
timer:sleep(500),
io:format("sundsvall status after adding  koping: \n"),
timer:sleep(500),
{sundsvall, adr()} ! {status, {lund, adr()}},
timer:sleep(500),

timer:sleep(500),
sundsvall ! broadcast,
timer:sleep(500),

stockholm ! update,
timer:sleep(500),
io:format("stockholm status after stockholm ! update FINAL: \n"),
{stockholm , adr()} ! {status, {stockholm, adr()}},
timer:sleep(500),
lund ! update,
timer:sleep(500), 
sundsvall ! update,
timer:sleep(500),
koping ! update,
timer:sleep(500),
io:format("koping final status : \n"),
{koping, adr()} ! {status, {koping, adr()}},
timer:sleep(500),
timer:sleep(500),


%sending message stockholm - sundsvall
io:format("sending message stockholm - sundsvall (again) \n"),
stockholm ! {send, sundsvall , hej}.

