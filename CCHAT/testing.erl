-module(testing).

-export([main/0]).
-record (server_state, {
    channels = []
}).

main() ->
    
    Channel = "#hej",
    list_to_atom(Channel).
