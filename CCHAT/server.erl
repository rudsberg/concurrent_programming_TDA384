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

handle(St, {join, Channel,Client}) ->

    ChannelAlreadyExist = (lists:member(Channel, St#server_state.channels)),
    if ChannelAlreadyExist ->
        Ans = (catch (genserver:request(list_to_atom(Channel), {join, Channel, Client}))),
        io:fwrite("In server joining existing channel ,  ~p\n", [Ans]),
        case Ans of 

            join ->  {reply,join,St};
            {error, user_already_joined, Msg}    -> {reply, {error, user_already_joined, Msg}, St};
            {error,server_not_reached}     -> {reply,{error, server_not_reached,"Server timed out."},St}
            end;
        true -> channel:start(list_to_atom(Channel),Client),
                NewState = St#server_state{channels = [Channel | St#server_state.channels]},
                {reply,join,NewState}
        end;

handle(St, {message_send, Channel,Client,Nick, Msg}) ->
   % io:fwrite("Sending message to ~p\n", [Channel]),
    Ans = (catch (genserver:request(list_to_atom(Channel), {message_send, Channel, Client,Nick,Msg}))),
    case Ans of 
        message_send -> {reply,message_send,St};
        'EXIT' -> {reply,{error, server_not_reached,"EXIT."},St}

    end,  
    {reply, message_send, St};

handle(St, {leave,Channel,Client}) ->
    ChannelExists = (lists:member(Channel, St#server_state.channels)),
    if 
    ChannelExists -> 
        Ans = (catch (genserver:request(list_to_atom(Channel),{leave,Channel,Client}))),
        case Ans of
            leave -> {reply,leave,St};
            {error,user_not_joined}    -> {reply,{error,user_not_joined,"User not in this channel."},St};
            {error,server_not_reached} -> {reply,{error,server_not_reached,"Server timed out."},St}
            end;
    true -> {reply,{error,user_not_joined,"User not in this channel."},St}
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
