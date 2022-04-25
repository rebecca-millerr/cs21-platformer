% A simple counter for tracking elapsed time in the game.
-module(tick_counter).

-export([start/0]).


count_loop(N) ->
    receive
        {increment} -> count_loop(N + 1);
        {report, Pid} -> 
            Pid ! {ticks, N},
            count_loop(N);
        {quit} -> ok
    end.


start() ->
    % TODO: handle error here
    {ok, _TRef} = timer:send_interval(125, {increment}), % 8 ticks / s
    count_loop(0).

