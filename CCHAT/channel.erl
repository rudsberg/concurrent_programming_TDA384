-module(channel).
-export([start/2,handle/2]).
%This module describes a channel and the actions that can be taken within a channel.

%State of channel, it only has a list of users(clients) in the channel currently.
-record (channel_state, {
    users = []
}).

%Start function, uses the genserver to return PID, register and start a receive loop using the
%handle/2 function. Initial state is a channel state with the user which started the channel in it.
start(ChannelAtom,User) ->
    genserver:start(ChannelAtom, #channel_state{users = [User]}, fun channel:handle/2).

%Handle taking care of the join command.
%Checks if the user is already in the channel, if it is, it returns an error with a corresponing
%error message. If the user is NOT in the channel, updates the channel state by adding the new user
%and returns indicating a successful request with the updated state. 
handle(St = #channel_state{users = Users}, {join, Client}) ->
        %Checking if user is in channel.
        AlreadyInChannel = (lists:member(Client, Users)),
        if AlreadyInChannel -> 
            {reply, {error, user_already_joined, "User already joined."}, St};
        true -> 
            NewState = St#channel_state{users = [Client | Users]},
                       {reply,join,NewState}
    end;
%Handle taking care of the leave command
%Checks if the user is in the channel, if not return a corresponding error message. If the user
%IS in the channel, update the channel state by deleting the user from the users list and returns
%indicating that the request was successful and returns an updated state. 
handle(St = #channel_state{users = Users}, {leave, Client}) ->

    UserInChannel = (lists:member(Client, Users)),
    if UserInChannel ->
        NewState = St#channel_state{users = lists:delete(Client,Users)},
        {reply,leave,NewState};
    true -> 
        {reply, {error, user_not_joined, "User not in this channel."}, St}
    end;

%Handle taking care of the message send command. If the user IS in the channel, the function
%will spawn new functions which tell the gui that they have a message to receive which in turn update the
%gui for all users in the channel which is not the sender.
%If the user is not in the channel, the function will return an errror with a corresponding error message.
handle(St = #channel_state{users = Users}, {message_send, Channel,Client,Nick,Msg}) ->
    UserInChannel = (lists:member(Client, Users)),
    if UserInChannel ->
        [spawn(fun () -> genserver:request((User), {message_receive,Channel,Nick,Msg}) end) || User <- Users, User /= Client], 
        {reply,message_send,St};
    true -> 
        {reply, {error, user_not_joined, "User can not send message if not in channel."}, St}
    end.





    


       

