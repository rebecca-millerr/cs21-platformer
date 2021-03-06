-module(canvas_state).

-export([start/0]).


server_loop(State) ->
    receive
        % blocks should be a JSON compatible map
        {place, Block} ->
            server_loop([ Block | State ]);
        {report, Pid} -> Pid ! {blocks, State}, server_loop(State);
        {broadcast} ->
            tick_counter ! {report, self()},
            receive
                {ticks, Ticks} ->
                    broadcaster ! {json, #{ticks => Ticks, blocks => State}}
            end,
            server_loop(State);
        _ -> server_loop(State)
    end.



start() -> 
    server_loop([]).
