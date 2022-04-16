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

% For handling decoded JSON data
handle_json(Json, State) ->
    case (maps:get(<<"type">>, Json, notype)) of
        notype -> Res = jsx:encode([{<<"error">>, <<"ill-defined request type">>}]),
                  {reply, {text, Res}, State};
        _      -> Res = jsx:encode([{<<"error">>, <<"unrecognized request type">>}]),
                  {reply, {text, Res}, State}
    end.

% State is [] on app startup
websocket_handle({text, Data}, State) ->
    case (jsx:is_json(Data)) of
        true ->  Map = jsx:decode(Data),
                 handle_json(Map, State);
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
