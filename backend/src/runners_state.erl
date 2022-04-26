% A utility that keeps track of the runners in the game

-module(runners_state).
-export([start/0]).


update_list(  _,   _, []) -> [];
update_list(ID, Pos, [ {ID, _} | Tail]) -> [ {ID, Pos} | Tail];
update_list(ID, Pos, [ {ID0, Pos0} | Tail]) -> 
    [ {ID0, Pos0} | update_list(ID, Pos, Tail)].

del_runner_pos( _, []) -> [];
del_runner_pos(ID, [{ID, _} | Tail]) -> Tail;
del_runner_pos(ID, [{ID0, Pos0} | Tail]) ->
    [{ID0, Pos0} | del_runner_pos(ID, Tail)].

prep_for_broadcast({ID, Pos}) -> jsx:encode(#{ID => Pos}).


% State is a map of Pids to runner IDs and a list of recent positions for
% each runner ID
server_loop({IDs, Poses}) ->
    receive
        {add_runner, Pid} ->
            ID = maps:size(IDs),
            NewIDs = maps:put(Pid, ID, IDs),
            Pid ! {id, ID},
            erlang:monitor(process, Pid),
            server_loop({NewIDs, [ {ID, #{x => 0, y => 0}} | Poses]});
        {update, ID, Pos} ->
            server_loop({IDs, update_list(ID, Pos, Poses)});
        {report, Pid} ->
            Pid ! {IDs, Poses},
            server_loop({IDs, Poses});
        {broadcast} ->
            broadcaster ! {message,
                lists:map(fun(X) -> prep_for_broadcast(X) end, Poses)},
            server_loop({IDs, Poses});
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("runner ~w ended for reason ~w~n", [Pid, Reason]),
            ID = maps:get(Pid, IDs),
            server_loop({maps:remove(Pid, IDs), del_runner_pos(ID, Poses)})
    end.


start() -> 
    server_loop({ #{}, [] }).
