% A utility that keeps track of the builders in the game

-module(builders_state).
-export([start/0]).



% State is a map of Pids to builder IDs
server_loop({Pids, Colors, NextID}) ->
    receive
        {add_builder, Pid} ->
            NewPids = maps:put(Pid, NextID, Pids),
            NewColors = maps:put(NextID, colors:random_rgb(), Colors),
            Pid ! {id, NextID},
            erlang:monitor(process, Pid),
            server_loop({NewPids, NewColors, NextID + 1});
        {report, Pid} ->
            Pid ! {builders, Pids, Colors},
            server_loop({Pids, Colors, NextID});
        {broadcast} ->
            tick_counter ! {report, self()},
            receive
                {ticks, Ticks} -> 
                    broadcaster ! {json, #{ticks => Ticks, builders => Colors}},
                    server_loop({Pids, Colors, NextID})
            end;
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, Pids),
            server_loop({maps:remove(Pid, Pids), maps:remove(ID, Colors), NextID})
    end.


start() -> 
    server_loop({#{}, #{}, 0}).
