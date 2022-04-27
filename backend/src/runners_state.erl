% A utility that keeps track of the runners in the game

-module(runners_state).
-export([start/0]).



% State is a map of Pids to runner IDs and a list of recent positions for
% each runner ID
server_loop({IDs, Poses, NextID}) ->
    receive
        {add_runner, Pid} ->
            Pid ! {id, NextID},
            erlang:monitor(process, Pid),
            server_loop({
                maps:put(Pid, NextID, IDs),
                maps:put(NextID, #{x => 0, y => 0}, Poses),
                NextID + 1});
        {update, ID, Pos} ->
            server_loop({IDs, maps:put(ID, Pos, Poses), NextID});
        {report, Pid} ->
            Pid ! {IDs, Poses},
            server_loop({IDs, Poses, NextID});
        {broadcast} ->
            broadcaster ! {json, #{runners => Poses}},
            server_loop({IDs, Poses, NextID});
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, IDs),
            server_loop({maps:remove(Pid, IDs), maps:remove(ID, Poses), NextID})
    end.


start() ->
    {ok, _TRef} = timer:send_interval(50, {broadcast}),
    server_loop({ #{}, #{}, 0 }).
