-module(backend_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Broadcaster = spawn(broadcaster, start, []),
    register(broadcaster, Broadcaster),

    TickCounter = spawn(tick_counter, start, []),
    register(tick_counter, TickCounter),

    Dispatch = cowboy_router:compile([
        {'_', [{"/", platformer_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

	backend_sup:start_link().

stop(_State) ->
	ok.
