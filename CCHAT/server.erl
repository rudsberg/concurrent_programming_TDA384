-module(server).

-export([start/1, stop/1,handle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.

-record (channel, {
    users = [],
    name
}).
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

handle(St = #server_state{channels = Channels}, {join, {Channel,Client}}) ->
    % If chat already exists, join that one, else create a new one 
    case lists:member(Channel, Channels) of
        true -> io:fwrite("Channel exists\n");
        _    -> St = #server_state{channels = [Channel | Channels]}
    end,
        
	  io:fwrite("joined channel ~p\n", [Channel]),
      {reply,ok,St};

handle(ServerState, {message_send, {Channel, Msg}}) ->
	  io:fwrite("Sent message ~p to channel ~p\n", [Msg, Channel]),
      {reply, ok, ServerState};

handle(ServerState, {leave, Channel}) ->
	io:fwrite("left channel ~p\n", [Channel]),
    ending.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
