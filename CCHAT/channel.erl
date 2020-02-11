-module(channel).
-export([start/2,handle/2,stop/1]).


-record (channel_state, {
    users = [],
    name
}).


start(ChannelAtom,User) ->
    genserver:start(ChannelAtom, #channel_state{users = [User]}, fun channel:handle/2).


handle(St, {join, Channel,Client}) ->
        io:fwrite("In channel joining  ~p\n", [Channel]),

        %Checking if user is in channel.
        AlreadyInChannel = (lists:member(Client, St#channel_state.users)),
        if AlreadyInChannel -> 
            {reply, {error, user_already_joined, "User already joined."}, St};

           true     -> NewState = St#channel_state{users = [Client | St#channel_state.users]},
                       % io:fwrite("Users in channel :\n",[NewState#channel_state.users]),
                       {reply,join,NewState}
    end;


handle(St, {leave, Client}) ->
    UserInChannel = (lists:member(Client, St#channel_state.users)),

    if UserInChannel ->
        NewState = St#channel_state{users = lists:delete(Client,St#channel_state.users)},
        {reply,leave,NewState};
    true -> 
        {reply, {error, user_not_joined, "User not in this channel."}, St}
    end;


handle(St, {message_send, Channel,Client,Nick,Msg}) ->
    UserInChannel = (lists:member(Client, St#channel_state.users)),
    if UserInChannel ->
        [spawn(fun () -> genserver:request((User), {message_receive,Channel,Nick,Msg}) end) || User <- St#channel_state.users, User /= Client], 
        {reply,message_send,St};
    true -> 
        {reply, {error, user_not_joined, "User can not send message if not in channel."}, St}
    end.

stop(Channel) ->
    Ans  = whereis(Channel),
    fwrite:io("Ans = ~p\n",[Ans]),
    case  Ans of
        undefined -> 
            already_stopped;
        Pid -> 
            exit(Pid,ok),
            ok
     end.






    


       

