-module(gms3).
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
    leader(Id, Master,0, [], [Master]).


%Starting a node should join an existing group
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
        
%init for en nod som vill joina en befintlig grupp.     
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        %Leader = <0.85.0>
        %slaves = <0.92.0>,<0.97.0>
        %Group = [<0.85.0>,<0.92.0>,<0.97.0>]
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            Last_message = {view, N, [Leader|Slaves], Group},
            
            slave(Id, Master, Leader,N+1,Last_message, Slaves,Group)

    after ?timeout ->
        %om vi inte far godkant av leader att joina
            Master ! {error, "no reply from leader"}
    end.

%The leader keeps the following state: 
%Id: a unique name, of the node, only used for debugging
%Master: the process identifier of the application layer
%Slaves: an ordered list of the process identifiers of all slaves in the group
%Group: a list of all application layer processes in the group
leader(Id, Master, N, Slaves, Group) ->
    receive
 % a message either from its own master or from a peer node
        {mcast, Msg} ->
            %bcast(Id, {msg, Msg}, Slaves),
            %bcast meddelande med dess nummer = N
            bcast(Id, {msg,N, Msg}, Slaves),
            Master ! Msg,
            %rekursivt anrop dar vi okar counter N... sa att nasta meddelande far ett unikt nummer
            leader(Id, Master,N+1, Slaves, Group);

%: a message, from a peer or the master, that is a request 
%from a node to join the group. The message contains both
%the process identifier of the application layer, Wrk, 
%and the process identifier of its group process.
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view,N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master,N+1, Slaves2, Group2);
        
        stop ->
        ok
    end.


%The slave is simply forwarding messages from its 
%master to the leader and vice verse
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
%a request from its master to multicast a message, the message is forwarded to the leader.
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader,N, Last, Slaves, Group);

%a request from the master to allow a new node 
%to join the group, the message is forwarded to the leader.    
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader,N,Last, Slaves, Group);

%ignoring duplicate messages
        {msg, I, _} when I < N ->
                slave(Id, Master, Leader, N, Last, Slaves, Group);
%a multicasted message from the leader. A message Msg
%is sent to the master.       
        {msg,N, Msg} ->
            Master ! Msg,
            %fatt ett nytt meddelande fran mastern som da
            %blir vart senaste 
            Last_message = {msg,N, Msg},
            slave(Id, Master, Leader,N+1 , Last_message, Slaves, Group);

%a multicasted view from the leader. A view
%is delivered to the master process   
        {view,N, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            %fatt ett nytt meddelande fran mastern som da
            %blir vart senaste 
            Last_message = {view,N, [Leader|Slaves2], Group2},
            slave(Id, Master, Leader, N+1 ,Last_message, Slaves2, Group2);

%'DOWN' comes from slaves monitor that is keeping an eye on the leader
%if the leader dies... go to election
        {'DOWN', _Ref, process, Leader, _Reason} ->
          election(Id, Master, N, Last, Slaves, Group);
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

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        %om det ar en sjalv som ligger forst...
        %da blir man leader 
        [Self|Rest] ->
            bcast(Id, Last, Rest),
            bcast(Id, {view,N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master,N+1, Rest, Group);
        %annars blir han som ligger forst leader 
        [Leader|Rest] ->
            %haller ett oga pa leadern
            %dor han blir det omvall
             erlang:monitor(process, Leader),
             slave(Id, Master, Leader,N, Last, Rest, Group)
    end.