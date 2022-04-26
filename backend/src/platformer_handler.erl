-module(platformer_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).



init(Req, State) ->
	{cowboy_websocket, Req, State, #{idle_timeout => 300000}}.

websocket_init(State) ->
    broadcaster ! {subscribe, self()},
    tick_counter ! {report, self()},
    canvas_state ! {report, self()},
    receive
        {ticks, Ticks} -> 
        receive
            {blocks, Blocks} ->
                {reply, {text, jsx:encode(
                    #{<<"ticks">> => Ticks,<<"blocks">> => Blocks})},
                State}
        end
    end.
	

% For handling casts based on decoded JSON data. 
% Sends a response only in the case of an invalid cast.
% TODO: Replace with console logging and never send a response
json_cast(Json, State) ->
    case (maps:get(<<"type">>, Json, notype)) of
        (<<"place">>) ->
          case(State) of
            {builder, ID} -> 
              canvas_state ! {place, #{builder => ID, pos => maps:get(<<"block">>, Json)}},
              tick_counter ! {report, self()},
                receive
                    {ticks, Ticks} ->
                        broadcaster  ! {json,
                            #{<<"ticks">> => Ticks,
                              <<"newblock">> => #{builder => ID, pos => maps:get(<<"block">>, Json)}}},
                        {ok, State}
                end;
            _  -> Res = jsx:encode([{<<"error">>, <<"Must be builder to place block">>}]),
                  {reply, {text, Res}, State}
          end;
        notype -> Res = jsx:encode([{<<"error">>, <<"Must specify cast type">>}]),
                  {reply, {text, Res}, State};
        Type   -> Res = jsx:encode([{<<"error">>, <<"unrecognized cast type">>}]),
                  io:format("~w~n~w~n", [Type, Json]),
                  {reply, {text, Res}, State}
    end.

% For handling calls based on decoded JSON data
json_call(Json, State) ->
    case (maps:get(<<"type">>, Json, notype)) of
        <<"become-builder">> -> 
            builders_state ! {add_builder, self()},
            tick_counter ! {report, self()},
            receive 
                {ticks, Ticks} -> 
                    receive
                        {id, ID} -> 
                            Res = jsx:encode(#{
                                <<"ticks">> => Ticks,
                                <<"id">> => ID
                            }),
                            {reply, {text, Res}, {builder, ID}}
                    end
            end;
        notype -> Res = jsx:encode([{<<"error">>, <<"Must specify call type">>}]),
                  {reply, {text, Res}, State};
        _      -> Res = jsx:encode([{<<"error">>, <<"unrecognized call type">>}]),
                  {reply, {text, Res}, State}
    end.


% State is [] on app startup
% Expects requests: 
%   - in JSON format. Easiest way is just to use JSON.stringify() in 
%     the websocket.send() call, as in websocket.send(JSON.stringify(obj))
%   - That "call" vs "cast" is specified, by the presence of one or the
%     other of those as a key. The value doesn't matter. e.g. 
%     { call: 1 } to send a call or { cast: 1 } to send a cast.
%   - Current plan is for there to also be a "type" field but I haven't
%     fleshed that out yet. e.g. something like {cast: 1, type: place_block}.
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

websocket_info(Info, State) ->
	{reply, {text, Info}, State}.
