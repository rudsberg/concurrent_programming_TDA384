-module(server).

-export([start/1, stop/1,handle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.

% -record (channel, {
%     users = [],
%     name
% }).
-record (server_state, {
    channels = []
}).


start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    %io:fwrite("server process started with id ~p~n",[self()]),
    %Pid = spawn(fun () -> startLoop() end),
    %register(ServerAtom, Pid),
    %Pid.
    genserver:start(ServerAtom, #server_state{}, fun server:handle/2).

handle(St, {join, {Channel,Client}}) ->
    
    IsMember = (lists:member(Channel, St#server_state.channels)),
    if IsMember ->
        Ans = (catch (genserver:request(list_to_atom(Channel), {join, {Channel, Client}}))),
        io:fwrite("In server joining existing channel ,  ~p\n", [Ans]),
        case Ans of 

            join ->  {reply,join,St};
            {error,user_already_joined}    -> {reply,{error, user_already_joined,"User already joined."},St};
            {error,server_not_reached}     -> {reply,{error, server_not_reached,"Server timed out."},St}
            end;
        true -> channel:start(list_to_atom(Channel),Client),
                NewState = St#server_state{channels = [Channel | St#server_state.channels]},
                {reply,join,NewState}
        end;



    % If chat already exists, join that one, else create a new one 
    % IsMember = (lists:member(Channel, St#server_state.channels)),
    
    % if IsMember -> io:fwrite("Channel exists\n"),
    %     %If channel already exists, check if user is in said chat.
    %     %Retrieve said channel.
    %     OldChannel = findValue(Channel,St#server_state.channels),
    %     %Checking if user is in said channel.
    %     Ans = (catch findValue(Client, OldChannel#channel.users)),
    %     case Ans of 
    %            %If we find the user, return error, saying the user is already in the channel.
    %            Val   ->  {reply,error_user_already_joined,"User already joined."};
    %            %If we dont find the user, its good, no worries :)
    %            %Add the user to the channel and return.
    %           error -> NewChannel = OldChannel#channel{users = [Client | OldChannel#channel.users]},
    %                    NewState = St#server_state{channels = [NewChannel | St#server_state.channels] },
    %                    {reply,join,NewState}
    %            end;

    %       true    -> io:fwrite("Channel doesn't exist\n"),
    %               NewState = St#server_state {channels = [Channel]} ,%St = #server_state{channels = [Channel | Channels]}
    %                 {reply,join,NewState}
    %      end;
	% %  io:fwrite("joined channel ~p\n", [Channel]),

handle(St, {message_send, {Channel,Client,Nick, Msg}}) ->
   % io:fwrite("Sending message to ~p\n", [Channel]),
    Ans = (catch (genserver:request(list_to_atom(Channel), {message_send, {Channel, Client,Nick,Msg}}))),
    case Ans of 
        message_send -> {reply,message_send,St};
        'EXIT' -> {reply,{error, server_not_reached,"EXIT."},St}

    end,  
    {reply, message_send, St};

handle(ServerState, {leave, Channel}) ->
	io:fwrite("left channel ~p\n", [Channel]),
    ending.

findValue(Val,[])->
    {error,"No value like that found."};

findValue(Val,[X | XS])->
    if Val == X ->
        X;
    true -> findValue(Val,XS)
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
