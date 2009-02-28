-module (test).

%% application callbacks
-export([start/2, start/0, layers_receive/1]).
-export ([test/0]).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

test() ->
  converse:send_message("0.0.0.0", {lookup, whisper:encrypt("a")}).

start(_Type, Config) ->
	io:format("Starting ~p with ~p~n", [?MODULE, Config]).


start() ->    
	layers:init(),
	layers:add(converse, [{port, 22002}]),
	layers:add(whisper, []),
	layers:add(where, []),
	layers:add(?MODULE, []),
	layers:start().

layers_receive(Msg) ->
  case Msg of
    {data, Socket, Data} ->
      io:format("Unencrypted in ~p data: ~p~n", [?MODULE, Data]),
      converse:reply(Socket, "Thanks!");
    Else -> 
      io:format("Received ~p in ~p~n", [Else, ?MODULE])
  end.
