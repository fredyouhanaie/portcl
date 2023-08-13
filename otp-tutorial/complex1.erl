
%% This is based on the module in the Erlang tutorials.
%% In fact, almost an exact copy of it!

-module(complex1).

-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1]).
-export([start/0]).

start() ->
    start(["./erlport.tcl"]).

start([ExtPrg]) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).

bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {complex, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("EXIT: Port=~p, Reason=~p.~n", [Port, Reason]),
            exit(port_terminated);
        ANY ->
            io:format("received/ignored unknown message >~p<~n", [ANY]),
            loop(Port)
    end.

encode({foo, X}) ->
    [1, X band 16#ff];

encode({bar, Y}) ->
    [2, Y band 16#ff].

decode([Int]) -> Int.
