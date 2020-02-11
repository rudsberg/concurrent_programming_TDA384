-module(server).

-export([start/1, stop/1,handle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.

% -record (channel, {
%     users = [],
%     name
% }).
-record (server_state, {
    channels = [],
    users    = []
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

handle(St, {join, Channel,Client,Nick}) ->
    UserState =checkIfUserInServer(Nick,St),
    io:fwrite("Users in server: ~p\n", [UserState]),

    ChannelAlreadyExist = (lists:member(Channel, St#server_state.channels)),
    if ChannelAlreadyExist ->
        io:fwrite("In server joining existing channel ,  ~p\n", [Channel]),
        case catch (genserver:request(list_to_atom(Channel), {join, Channel, Client})) of 
            join                                -> {reply,join,UserState};
            {error, user_already_joined, Msg}   -> {reply, {error, user_already_joined, Msg}, UserState};
            {'EXIT', Msg}                       -> {reply, {error, server_not_reached, Msg}, UserState}
        end;
        true -> channel:start(list_to_atom(Channel),Client),
                NewState = UserState#server_state{channels = [Channel | UserState#server_state.channels]},
                {reply,join,NewState}
        end;

handle(St,{nick,OldNick,NewNick}) ->
    NewNickExists = lists:member(NewNick,St#server_state.users),
    if 
        NewNickExists -> {reply,{error,nick_taken,"Nick already in use, please try another."},St};
        true          -> UserListWithoutOldNick = lists:delete(OldNick,St#server_state.users),
                         NewState = St#server_state{users = [NewNick | UserListWithoutOldNick]},
                         {reply,nick,NewState}
                         end;

%handle(St, {message_send, Channel,Client,Nick, Msg}) ->
%    case catch (genserver:request(list_to_atom(Channel), {message_send, Channel, Client,Nick,Msg})) of 
%        message_send -> {reply,message_send,St};
%        {error, user_not_joined, ErrorMsg} -> {reply, {error, user_not_joined, ErrorMsg}, St};
%        'EXIT' -> {reply,{error, server_not_reached,"EXIT."},St}
%    end;

%handle(St, {leave,Channel,Client}) ->
%    ChannelExists = (lists:member(Channel, St#server_state.channels)),
%    if ChannelExists -> 
%        case catch (genserver:request(list_to_atom(Channel), {leave, Client})) of
%            leave -> {reply,leave,St};
%            {error,user_not_joined, ErrorMsg}    -> {reply, {error,user_not_joined, ErrorMsg},St};
%            {error,server_not_reached} -> {reply,{error,server_not_reached,"Server timed out."},St}
%        end;
%    true -> 
%        {reply,{error,user_not_joined,"User not in this channel."},St}
%    end;
handle(St,{add_user, User}) ->
    NewState = St#server_state{users = [User | St#server_state.users]},
    {reply,add_user,NewState};





handle(St,stop_channels) ->
    Results = [genserver:stop(list_to_atom(Ch)) || Ch <- St#server_state.channels],
    Predicate = fun (E) ->E == ok end,
    Status = lists:all(Predicate,Results),

    if Status ->    {reply,stop_channels,St};
        true ->   {error,{server_not_reached,"Server not reached."},#server_state{}}
        end.

checkIfUserInServer(Client,St) ->
UserInServer = lists:member(Client,St#server_state.users),
    if UserInServer ->
        St;
        true -> NewState = St#server_state{users = [Client | St#server_state.users]}
    end.
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    case  (catch (genserver:request(ServerAtom,stop_channels))) of
        stop_channels -> genserver:stop(ServerAtom);
        _          -> {reply,{error,server_not_reached,"Timed out."},#server_state{}}
    end.

