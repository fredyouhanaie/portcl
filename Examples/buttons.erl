
%%% buttons.erl
%%%
%%% Launch two instances of the buttons.tcl script.
%%% Wait for messages from the ports and print as text.
%%% Quit when 2 port EXIT messages are received.
%%%

-module(buttons).
-export([start/0]).

-define(TclCmd, {spawn_executable, "buttons.tcl"}).

start() ->
	erlang:open_port(?TclCmd, [ {line, 100} ]),
	erlang:open_port(?TclCmd, [ {line, 100} ]),
	loop(2).

loop(0) ->
	io:format("no more ports, signing off!~n"),
	ok;

loop(N) ->
	receive
		{Port, {data, {eol, Msg}}} when is_port(Port) ->
			io:format("Port ~p: received ~p.~n", [Port, Msg]),
			loop(N);
		{'EXIT', Port, normal} when is_port(Port) ->
			io:format("Port ~p: received EXIT.~n", [Port]),
			loop(N-1);
		ANY ->
			io:format("unexpected message: ~p.~n", [ANY]),
			loop(N)
	after 5000 ->
		io:format("TICK~n"),
		loop(N)
	end.

