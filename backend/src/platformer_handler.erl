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

% For handling casts based on decoded JSON data. 
% Sends a response only in the case of an invalid cast.
% TODO: Replace with console logging and never send a response
json_cast(Json, State) ->
    case (maps:get(<<"type">>, Json, notype)) of
        notype -> Res = jsx:encode([{<<"error">>, <<"Must specify cast type">>}]),
                  {reply, {text, Res}, State};
        _      -> Res = jsx:encode([{<<"error">>, <<"unrecognized cast type">>}]),
                  {reply, {text, Res}, State}
    end.

% For handling calls based on decoded JSON data
json_call(Json, State) ->
    case (maps:get(<<"type">>, Json, notype)) of
        notype -> Res = jsx:encode([{<<"error">>, <<"Must specify call type">>}]),
                  {reply, {text, Res}, State};
        _      -> Res = jsx:encode([{<<"error">>, <<"unrecognized call type">>}]),
                  {reply, {text, Res}, State}
    end.


% State is [] on app startup
websocket_handle({text, Data}, State) ->
    case (jsx:is_json(Data)) of
        true ->  Map = jsx:decode(Data),
                 Call = maps:is_key(<<"call">>, Map),
                 Cast = maps:is_key(<<"cast">>, Map),
                 if
                     Call -> json_call(Map, State);
                     Cast -> json_cast(Map, State);
                     true -> Res = jsx:encode([{<<"error">>, <<"Must specify call or cast">>}]),
                             {reply, {text, Res}, State}
                 end;
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
