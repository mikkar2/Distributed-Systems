-module(gms1).
-compile(export_all).

%initializing a provcess that is the first node in the group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).


%Starting a node should join an existing group
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
        
    
init(Id, Grp, Master) ->
    %Self = group-process
    Self = self(),
    %Master = application level
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
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
        
        stop ->
            ok
    end.

bcast(_, _ , []) -> ok;
bcast(Id, Message, [H | T]) ->
    H ! Message,
    bcast(Id , Message, T).
