-module(channel).
-export([start/2,handle/2]).


-record (channel_state, {
    users = [],
    name
}).


start(ChannelAtom,User) ->
    genserver:start(ChannelAtom, #channel_state{users = [User]}, fun channel:handle/2).


handle(St, {join, {Channel,Client}}) ->
        io:fwrite("In channel joining  ~p\n", [Channel]),

        %Checking if user is in channel.
        IsMember = (lists:member(Client, St#channel_state.users)),
        if IsMember -> {reply,error_user_already_joined,"User already joined."};

           true     -> NewState = St#channel_state{users = [Client | St#channel_state.users]},
                       % io:fwrite("Users in channel :\n",[NewState#channel_state.users]),
                       {reply,join,NewState}
    end;


handle(St, {leave, {Channel,Client,Nick}}) ->
    {reply,leave,St};

handle(St, {message_send, {Channel,Client,Nick,Msg}}) ->
    
   [spawn(fun () -> genserver:request((User), {message_receive,Channel,Nick,Msg}) end) || User <- St#channel_state.users, User /= Client],
   {reply,message_send,St}.






   %spawn(fun () -> genserver:request(list_to_atom(User), {message_receive,{Channel,Nick,Msg}}) end).





%         Ans = (catch (genserver:request(list_to_atom(Client), {message_receive, {Channel,Nick,Msg}}))),
%         io:fwrite("Ans = ~p\n", [Ans]),

%        % case Ans of 
%    %     message_receive -> {reply,message_send,St};
%     %    {error,_} -> {error,{error_user_already_joined,"User not in channel lul."},St}
%     %    end;
%        % true -> {error,{error_user_already_joined,"User not in channel lul."},St}
%    %     end.
%   Pid = spawn(fun() -> loop(State, F) end),

%  {reply,message_send,St}.




    


       

