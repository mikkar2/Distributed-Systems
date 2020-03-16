-module(gms2).
-compile(export_all).

-define(arghh, 100).
-define(timeout, 2000).

%initializing a provcess that is the first node in the group
start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self, dum) end)}.

init(Id, Rnd, Master, dum) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).


%Starting a node should join an existing group
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
        
    
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        %Leader = <0.85.0>
        %slaves = <0.92.0>,<0.97.0>
        %Group = [<0.85.0>,<0.92.0>,<0.97.0>]
        {view, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            io:format("SLaves: ~p \n" , [Slaves]),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)

    after ?timeout ->
        %om vi inte far godkant av leader att joina
            Master ! {error, "no reply from leader"}
    end.

%The leader keeps the following state: 
%Id: a unique name, of the node, only used for debugging
%Master: the process identifier of the application layer
%Slaves: an ordered list of the process identifiers of all slaves in the group
%Group: a list of all application layer processes in the group
leader(Id, Master, Slaves, Group) ->
    receive
 % a message either from its own master or from a peer node
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);

%: a message, from a peer or the master, that is a request 
%from a node to join the group. The message contains both
%the process identifier of the application layer, Wrk, 
%and the process identifier of its group process.
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        
        stop ->
        ok
    end.


%The slave is simply forwarding messages from its 
%master to the leader and vice verse
slave(Id, Master, Leader, Slaves, Group) ->
    receive
%a request from its master to multicast a message, the message is forwarded to the leader.
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);

%a request from the master to allow a new node 
%to join the group, the message is forwarded to the leader.    
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);

%a multicasted message from the leader. A message Msg
%is sent to the master.       
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);

%a multicasted view from the leader. A view
%is delivered to the master process   
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);

%'DOWN' comes from slaves monitor that is keeping an eye on the leader
%if the leader dies... go to election
        {'DOWN', _Ref, process, Leader, _Reason} ->
          election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.

%bcast(_, _ , []) -> ok;
%bcast(Id, Message, [H | T]) ->
    %H ! Message,
   % bcast(Id , Message, T).

%To show that it is not working
%we can change the bcast/3 procedure and introduce a random crash. We
%define a constant arghh that defines the risk of crashing. A value of 100
%means that a process will crash in average once in a hundred attempts. The
%definition of bcast/3 now looks like this:

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
        ok
     end.


%In the election state the process will select the first node in its lists of
%peers and elect this as the leader. It could of course be that the process finds
%itself being the first node and it will thus become the leader of the group

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        %om det ar en sjalv som ligger forst...
        %da blir man leader 
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        %annars blir han som ligger forst leader 
        [Leader|Rest] ->
            %haller ett oga pa leadern
            %dor han blir det omvall
             erlang:monitor(process, Leader),
             slave(Id, Master, Leader, Rest, Group)
    end.