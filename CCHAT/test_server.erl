-module(test_server).

-export([start/1]).





start([]) -> 
    spawn(fun () -> loop(0) end).     

loop(N) ->
    receive
        {multiply, N1, N2} -> io:fwrite("Mult res is ~p", [N1*N2])
    end,
    loop(N+1).

stop(Server) ->
    Server ! {stop, self(), 0},
    ok.