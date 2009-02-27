-module (test).

%% application callbacks
-export([start/0, layers_receive/1]).
-export ([test/0]).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

test() ->
  converse:send("0.0.0.0", {data, "hey you guys"}).
  
start() ->    
	layers:init(),
	layers:add(converse, [{port, 22002}]),
	layers:add(whisper, []),
	layers:add(where, []),
	layers:add(layers_test_app, []),
	layers:start().

layers_receive(Msg) ->
  case Msg of
    {data, Socket, Data} ->
      io:format("Unencrypted in ~p data: ~p~n", [?MODULE, Data]),
      converse:reply(Socket, "Thanks!")
  end.