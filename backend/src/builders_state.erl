% A utility that keeps track of the builders in the game

-module(builders_state).
-export([start/0]).


del_list(_, []) -> [];
del_list(X, [X | Xs]) -> Xs;
del_list(X, [Y | Xs]) -> [Y | del_list(X, Xs)].

% State is a map of Pids to builder IDs
server_loop({Pids, IDs, NextID}) ->
    receive
        {add_builder, Pid} ->
            NewPids = maps:put(Pid, NextID, Pids),
            Pid ! {id, NextID},
            erlang:monitor(process, Pid),
            server_loop({NewPids, [NextID | IDs], NextID + 1});
        {report, Pid} ->
            Pid ! {builders, Pids, IDs},
            server_loop({Pids, IDs, NextID});
        {broadcast} ->
            tick_counter ! {report, self()},
            receive
                {ticks, Ticks} -> 
                    broadcaster ! {json, #{ticks => Ticks, builders => IDs}},
                    server_loop({Pids, IDs, NextID})
            end;
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, Pids),
            server_loop({maps:remove(Pid, Pids), del_list(ID, IDs), NextID})
    end.


start() -> 
    server_loop({#{}, [], 0}).
