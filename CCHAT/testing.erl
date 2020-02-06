-module(testing).

-export([main/0]).


main() ->
    lists:member(1, [1,2,3]).