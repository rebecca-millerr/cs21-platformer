% A utility that keeps track of the runners in the game

-module(runners_state).
-export([start/0]).



% State is a map of Pids to runner IDs and a list of recent positions for
% each runner ID
server_loop({IDs, Poses}) ->
    receive
        {add_runner, Pid} ->
            ID = maps:size(IDs),
            NewIDs = maps:put(Pid, ID, IDs),
            Pid ! {id, ID},
            erlang:monitor(process, Pid),
            server_loop({NewIDs, maps:put(ID, #{x => 0, y => 0}, Poses)});
        {update, ID, Pos} ->
            server_loop({IDs, maps:put(ID, Pos, Poses)});
        {report, Pid} ->
            Pid ! {IDs, Poses},
            server_loop({IDs, Poses});
        {broadcast} ->
            broadcaster ! {json, #{runners => Poses}},
            server_loop({IDs, Poses});
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, IDs),
            server_loop({maps:remove(Pid, IDs), maps:remove(ID, Poses)})
    end.


start() -> 
    % for now just send every 2 seconds, obv need to make much more often soon
    {ok, _TRef} = timer:send_interval(2000, {broadcast}),
    server_loop({ #{}, #{} }).
