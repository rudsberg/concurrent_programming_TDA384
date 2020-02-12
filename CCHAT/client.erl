-module(client).
-export([handle/2, initial_state/3]).


-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.
        
%Sends a join request to the process registered as ServerAtom and acts according to 
%the result.
handle(St = #client_st{server = ServerAtom,nick = Nick}, {join, Channel}) ->
    case catch (genserver:request(ServerAtom, {join, Channel, self(),Nick})) of
        join                              -> {reply, ok, St};
        {error,user_already_joined, Msg}  -> {reply, {error, user_already_joined, Msg}, St};
        {error,server_not_reached, Msg}   -> {reply, {error, server_not_reached,  Msg}, St};
        {'EXIT', Msg}                     -> {reply, {error, server_not_reached,  Msg}, St}
    end;

%Sends a request to the process registered as Channel trying to join a specific channel
% and acts according to the result.
handle(St, {leave, Channel}) ->
            case catch (genserver:request(list_to_atom(Channel), {leave, self()})) of
        leave ->    {reply,ok,St};
        {error, server_not_reached}   -> {reply, {error, server_not_reached, "Server timed out."}, St};
        {error, user_not_joined, ErrorMsg}   -> {reply, {error, user_not_joined, ErrorMsg}, St}
    end;

%Sends a request to the process registered as Channel trying to send a message to the users in that specific channel
%and acts according to the result.
handle(St = #client_st{nick = Nick}, {message_send, Channel, Msg}) ->
    case catch (genserver:request(list_to_atom(Channel), {message_send, Channel, self(),Nick,Msg})) of 
        message_send -> {reply,ok,St};
        {error, user_not_joined, ErrorMsg} -> {reply, {error, user_not_joined, ErrorMsg}, St};
        _   -> {reply,{error,server_not_reached, "Server timed out."},St}
    end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
%Sends request to the process registered as ServerAtom trying to change the nick of the client
% and acts according to the result.
handle(St = #client_st{server = ServerAtom, nick = Nick}, {nick, NewNick}) ->
    case catch (genserver:request(ServerAtom,{nick,Nick ,NewNick})) of
        nick                           -> NewState = St#client_st{nick = NewNick},{reply,ok,NewState};
        {error,nick_taken,ErrorMsg}    -> {reply,{error,nick_taken,ErrorMsg},St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
