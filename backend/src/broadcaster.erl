-module(broadcaster).

-export([start/0]).


broadcast_message(_Message, []) -> ok;
broadcast_message(Message, [Pid | Pids]) ->
    Pid ! Message,
    broadcast_message(Message, Pids).

% does not change the list nor send an error if the pid is not found
remove_subscriber(_Pid, []) -> [];
remove_subscriber(Pid,  [Pid  | Pids]) -> Pids;
remove_subscriber(Pid, [Pid0 | Pids]) -> [Pid0 | remove_subscriber(Pid, Pids) ].


server_loop(State) ->
    receive
        {subscribe, Pid} ->
            erlang:monitor(process, Pid),
            server_loop([ Pid | State]);
        {message, Message} ->
            broadcast_message(Message, State),
            server_loop(State);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            server_loop(remove_subscriber(Pid, State));
        {reportstate, Pid} -> 
            Pid ! State,
            server_loop(State);
        {quit} -> ok;
        _ -> server_loop(State)
    end.

start() -> 
    server_loop([]).
