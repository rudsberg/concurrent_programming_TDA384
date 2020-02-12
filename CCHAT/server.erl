-module(server).

-export([start/1, stop/1,handle/2]).

-record (server_state, {
    channels = [],
    users    = []
}).

start(ServerAtom) ->
    genserver:start(ServerAtom, #server_state{}, fun server:handle/2).

handle(St, {join, Channel,Client,Nick}) ->
    UserState = addNewUserToServerState(Nick,St),

    ChannelAlreadyExist = (lists:member(Channel, St#server_state.channels)),
    if ChannelAlreadyExist ->
        case catch (genserver:request(list_to_atom(Channel), {join, Client})) of 
            join                                -> {reply,join,UserState};
            {error, user_already_joined, Msg}   -> {reply, {error, user_already_joined, Msg}, UserState};
            {'EXIT', Msg}                       -> {reply, {error, server_not_reached, Msg}, UserState}
        end;
        true -> channel:start(list_to_atom(Channel),Client),
                NewState = UserState#server_state{channels = [Channel | UserState#server_state.channels]},
                {reply,join,NewState}
        end;

handle(St = #server_state{users = Users},{nick,OldNick,NewNick}) ->
    NewNickExists = lists:member(NewNick,St#server_state.users),
    if NewNickExists  -> 
        {reply,{error,nick_taken,"Nick already in use, please try another."},St};
       
    true ->
        UserListWithoutOldNick = lists:delete(OldNick,Users),
        NewState = St#server_state{users = [NewNick | UserListWithoutOldNick]},
        {reply,nick,NewState}
    end;

handle(St = #server_state {users = Users},{add_user, User}) ->
    NewState = St#server_state{users = [User | Users]},
    {reply,add_user,NewState};

handle(St = #server_state {channels = Channels},stop_channels) ->  
    ShutDownChannelsWithStatus = lists:zip([genserver:stop(list_to_atom(Ch)) || Ch <- Channels], Channels),
    NotShutdown = [Ch || {Status, Ch} <- ShutDownChannelsWithStatus, Status /= ok],

    if length(NotShutdown) == 0 ->    
        {reply,stop_channels,St};
    true ->           
        {error, {server_not_reached,"Server not reached."}, St#server_state{channels = NotShutdown}}
    end.

addNewUserToServerState(Client,St = #server_state{users = Users}) ->
UserInServer = lists:member(Client,Users),
    if UserInServer ->
        St;
        true -> St#server_state{users = [Client | Users]}
    end.


stop(ServerAtom) ->
    case  (catch (genserver:request(ServerAtom,stop_channels))) of
        stop_channels -> genserver:stop(ServerAtom);
        _          -> {reply,{error,server_not_reached,"Timed out."},#server_state{}}
    end.

