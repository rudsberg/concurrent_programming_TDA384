-module(channel).
-export([start/2,handle/2]).


-record (channel_state, {
    users = [],
    name
}).


start(ChannelAtom,User) ->
    genserver:start(ChannelAtom, #channel_state{users = [User]}, fun channel:handle/2).


handle(St = #channel_state{users = Users}, {join, Client}) ->

        %Checking if user is in channel.
        AlreadyInChannel = (lists:member(Client, Users)),
        if AlreadyInChannel -> 
            {reply, {error, user_already_joined, "User already joined."}, St};

           true     -> NewState = St#channel_state{users = [Client | Users]},
                       {reply,join,NewState}
    end;


handle(St = #channel_state{users = Users}, {leave, Client}) ->
    UserInChannel = (lists:member(Client, Users)),

    if UserInChannel ->
        NewState = St#channel_state{users = lists:delete(Client,Users)},
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





    


       

