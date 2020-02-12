-module(server).

-export([start/1, stop/1,handle/2]).

-record (server_state, {
    channels = [],
    nicks    = []
}).

start(ServerAtom) ->
    genserver:start(ServerAtom, #server_state{}, fun server:handle/2).

% If channel does not exist before, a new channel will get created and client
% will join this newly created channel. If channel exist and client is not 
% already apart of it, it will join it. The nick is added to server state 
% to keep track of all taken nicks.
handle(St, {join, Channel, Client, Nick}) ->
    NewState = addNickIfNotAddedBefore(St, Nick),
    ChannelAlreadyExist = (lists:member(Channel, St#server_state.channels)),

    if ChannelAlreadyExist ->
        case catch (genserver:request(list_to_atom(Channel), {join, Client})) of 
            join                                -> {reply, join, NewState};
            {error, user_already_joined, Msg}   -> {reply, {error, user_already_joined, Msg}, NewState};
            {'EXIT', Msg}                       -> {reply, {error, server_not_reached, Msg}, NewState}
        end;
    true -> 
        channel:start(list_to_atom(Channel),Client),
        _NewState = NewState#server_state{channels = [Channel | NewState#server_state.channels]},
        {reply, join, _NewState}
    end;


handle(St = #server_state{nicks = Nicks}, {nick, OldNick, NewNick}) ->
    NewNickExists = lists:member(NewNick, St#server_state.nicks),
    if NewNickExists  -> 
        {reply,{error,nick_taken,"Nick already in use, please try another."},St};
    true ->
        UserListWithoutOldNick = lists:delete(OldNick, Nicks),
        NewState = St#server_state{nicks = [NewNick | UserListWithoutOldNick]},
        {reply, nick, NewState}
    end;

handle(St = #server_state {nicks = Nicks}, {add_user, User}) ->
    NewState = St#server_state{nicks = [User | Nicks]},
    {reply,add_user,NewState};

handle(St = #server_state {channels = Channels},stop_channels) ->  
    ShutDownChannelsWithStatus = lists:zip([genserver:stop(list_to_atom(Ch)) || Ch <- Channels], Channels),
    NotShutdown = [Ch || {Status, Ch} <- ShutDownChannelsWithStatus, Status /= ok],

    if length(NotShutdown) == 0 ->    
        {reply,stop_channels,St};
    true ->           
        {error, {server_not_reached,"Server not reached."}, St#server_state{channels = NotShutdown}}
    end.

addNickIfNotAddedBefore(St = #server_state{nicks = Nicks}, Nick) ->
    UserInServer = lists:member(Nick, Nicks),
    if UserInServer ->
        St;
    true -> 
        St#server_state{nicks = [Nick | Nicks]}
    end.


stop(ServerAtom) ->
    case  (catch (genserver:request(ServerAtom,stop_channels))) of
        stop_channels -> genserver:stop(ServerAtom);
        _          -> {reply,{error,server_not_reached,"Timed out."},#server_state{}}
    end.

