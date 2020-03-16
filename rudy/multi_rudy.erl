-module(multi_rudy).
-export([start/2, stop/0]).

%registrer a process under the name multi_rudy
start(Port, Number_of_workers) ->
    register(multi_rudy, spawn(fun() -> init(Port, Number_of_workers) end)).

stop() ->
        multi_rudy ! terminate.

%init(Port): the procedure that will initialize the server, takes a port
%number (for example 8080), opens a listening socket and passes the
%socket to handler/1. Once the request has been handled the socket
%will be closed.

%gen tcp:listen(Port, Option): this is how a listening socket is
%opened by the server. We will pass the port number as an argument
%and use the following option: list, {active, false}, {reuseaddr,
%true}. Using these option we will see the bytes as a list of integers
%instead of a binary structure. We will need to read the input using
%recv/2 rather than having it sent to us as messages. The port address
%should be used again and again.


init(Port , Number_of_workers) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
        worker(Listen, Number_of_workers),
        waiting();
        %gen_tcp:close(Listen),
        %ok;
        {error, Error} ->
        error
    end.

waiting() ->
        io:fwrite("waiting \n"),
        receive
        terminate ->
            io:fwrite("terminate program \n"),
            exit(whereis(multi_rudy), "time to die")
        end.


%a process is created with spawn
worker(Listen , Number_of_workers) when Number_of_workers == 0 -> ok;
worker(Listen , Number_of_workers) when 
Number_of_workers > 0 -> 
spawn(fun() -> handler(Listen, Number_of_workers) end),     %a process is created by calling spawn.
io:fwrite("worker ~p created\n" , [Number_of_workers]), 
worker(Listen, Number_of_workers - 1).


    


%handler(Listen): will listen to the socket for an incoming connection.
%Once a client has connect it will pass the connection to request/1.
%When the request is handled the connection is closed.

%gen tcp:accept(Listen): this is how we accept an incoming request.
%If it succeeds we will have a communication channel open to the client.

handler(Listen , Worker_id) ->
    %io:fwrite("handler: ~p \n" , [Worker_id]),
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
        request(Client),
        handler(Listen, Worker_id);
        {error, Error} ->
        error
    end.

%request(Client): will read the request from the client connection
%and parse it. It will then parse the request using your http parser and
%pass the request to reply/1. The reply is then sent back to the client.

%gen tcp:recv(Client, 0): once we have the connection to the Client
%we will read the input and return it as a string. The 0 augment 0, tells
%the system to read as much as possible.

%gen tcp:send(Client, Reply): this is how we send back a reply, in
%the form of a string, to the client.

%gen tcp:close(Socket): once we are done we need to close the connection. Note that we also have to close the listening socket that we
%opened in the beginning.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
        Request = http:parse_request(Str),
        Response = reply(Request),
        gen_tcp:send(Client, Response);
        {error, Error} ->
        io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

%reply(Request): this is where we decide what to reply, how to turn
%the reply into a well formed HTTP reply.

reply({{get, URI, _}, _, _}) ->
%timer:sleep(40),
http:ok(URI).