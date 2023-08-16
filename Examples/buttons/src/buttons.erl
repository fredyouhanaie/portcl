%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% Launch two instances of the buttons.tcl script.
%%% Wait for messages from the ports and print as text.
%%% Quit when 2 port EXIT messages are received.
%%%
%%% @end
%%% Created : 13 Aug 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(buttons).

%% API exports
-export([main/1]).
-export([start/0]).

%%--------------------------------------------------------------------

-define(TclCmd, {spawn_executable, "priv/buttons.tcl"}).
-define(TclArgs, ["-2"]).
-define(PortMode, {packet, 2}).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    erlang:open_port(?TclCmd, [ ?PortMode, {args, ?TclArgs} ]),
    erlang:open_port(?TclCmd, [ ?PortMode, {args, ?TclArgs} ]),
    loop(2).

%%--------------------------------------------------------------------
%% escript Entry point
%%
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    start(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

loop(0) ->
    io:format("no more ports, signing off!~n"),
    ok;

loop(N) ->
    receive
        {Port, {data, Msg}} when is_port(Port) ->
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

%%--------------------------------------------------------------------
