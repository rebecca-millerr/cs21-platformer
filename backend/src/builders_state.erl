% A utility that keeps track of the builders in the game

-module(builders_state).
-export([start/0]).



% State is a map of Pids to builder IDs
server_loop({Pids, Colors}) ->
    receive
        {add_builder, Pid} ->
            ID = maps:size(Pids),
            NewPids = maps:put(Pid, ID, Pids),
            NewColors = maps:put(ID, colors:random_rgb(), Colors),
            Pid ! {id, ID},
            erlang:monitor(process, Pid),
            server_loop({NewPids, NewColors});
        {report, Pid} ->
            Pid ! {builders, Pids, Colors},
            server_loop({Pids, Colors});
        {broadcast} ->
            tick_counter ! {report, self()},
            receive
                {ticks, Ticks} -> 
                    broadcaster ! {json, #{ticks => Ticks, builders => Colors}},
                    server_loop({Pids, Colors})
            end;
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, Pids),
            server_loop({maps:remove(Pid, Pids), maps:remove(ID, Colors)})
    end.


start() -> 
    server_loop({#{}, #{}}).
