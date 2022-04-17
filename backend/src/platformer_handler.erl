-module(platformer_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	{ok, State}.

websocket_handle({text, Json}, State) ->
    case (jsx:is_json(Json)) of
        true ->  Map = jsx:decode(Json),
                 io:format("received JSON: ~w~n", [Map]),
	             {reply, {text, Json}, State};
        false -> Res = jsx:encode([{<<"error">>, <<"Message is not valid JSON">>}]),
                 {reply, {text, Res}, State}
    end;
websocket_handle({binary, Data}, State) ->
    io:format("received binary: ~w~n", [Data]),
	{reply, {binary, Data}, State};
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.
