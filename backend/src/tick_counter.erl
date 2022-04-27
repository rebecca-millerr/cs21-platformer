% A simple counter for tracking elapsed time in the game.
-module(tick_counter).

-export([start/0]).


count_loop(N) ->
    receive
        {increment} -> count_loop(N + 1);
        {report, Pid} ->
            Pid ! {ticks, N},
            count_loop(N);
        {broadcast} ->
            broadcaster ! {json, #{ticks => N}},
            count_loop(N);
        {quit} -> ok
    end.


start() ->
    % TODO: handle errors here?
    {ok, _TRefIncr} = timer:send_interval(125, {increment}), % 8 ticks / s
    {ok, _TRefBdct} = timer:send_interval(250, {broadcast}), % 4 broadcasts / s
    count_loop(0).

