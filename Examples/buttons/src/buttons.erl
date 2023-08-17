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
-export([start/0, start/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

-define(TclCmd, {spawn_executable, "priv/buttons.tcl"}).
-define(TclArgs, ["-2"]).
-define(PortMode, {packet, 2}).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    start([]).

%%--------------------------------------------------------------------

start(Args) ->
    logger:set_primary_config(#{level => notice}),
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),

    ?LOG_NOTICE("Args=~p.", [Args]),

    erlang:open_port(?TclCmd, [ ?PortMode, {args, ?TclArgs}, exit_status ]),
    erlang:open_port(?TclCmd, [ ?PortMode, {args, ?TclArgs}, exit_status ]),
    loop(2).

%%--------------------------------------------------------------------
%% escript Entry point
%%
main(Args) ->
    start(Args),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

loop(0) ->
    ?LOG_NOTICE("no more ports, signing off!"),
    ok;

loop(N) ->
    receive
        {Port, {data, Msg}} when is_port(Port) ->
            ?LOG_NOTICE("Port ~p: received ~p.", [Port, Msg]),
            loop(N);
        {Port, {exit_status, Status}} when is_port(Port) ->
            ?LOG_NOTICE("Port ~p: terminated, status=~p.", [Port, Status]),
            loop(N-1);
        ANY ->
            ?LOG_NOTICE("unexpected message: ~p.", [ANY]),
            loop(N)
    after 5000 ->
            ?LOG_NOTICE("TICK"),
            loop(N)
    end.

%%--------------------------------------------------------------------
