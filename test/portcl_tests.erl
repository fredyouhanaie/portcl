%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, fy
%%% @doc
%%%
%%% Test module for portcl.tcl
%%%
%%% @end
%%% Created : 16 Aug 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(portcl_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

-define(TclCmd, {spawn_executable, "test/portcl_tests.tcl"}).
-define(TclEnv, {env, [ {"TCLLIBPATH", "priv"} ]}).

%%--------------------------------------------------------------------

do_open(PortMode, Args) ->
    open_port(?TclCmd, [PortMode, ?TclEnv, {args, Args}]).

%%--------------------------------------------------------------------

open1() -> do_open({packet, 1}, ["-1"]).
open2() -> do_open({packet, 2}, ["-2"]).
open4() -> do_open({packet, 4}, ["-4"]).
openl() -> do_open({line, 100}, ["-l"]).
opens() -> do_open(stream,      ["-s"]).

%%--------------------------------------------------------------------

send(Port, Msg) -> port_command(Port, Msg).

%%--------------------------------------------------------------------

wait4data(P, Data, Timeout) ->
    receive
        {P, {data, Data}} ->
            ok
    after Timeout ->
            timeout
    end.

%%--------------------------------------------------------------------

open1_test() -> test_pack(open1()).
open2_test() -> test_pack(open2()).
open4_test() -> test_pack(open4()).
openl_test() -> test_line(openl()).
opens_test() -> test_pack(opens()).

%%--------------------------------------------------------------------

test_pack(P) ->
    ?assert( is_port(P) ),
    ?assertEqual( wait4data(P, "ok", 3000), ok ),
    send(P, "hello"),
    ?assertEqual( wait4data(P, "hello", 3000), ok ),
    send(P, "goodbye"),
    ?assertEqual( wait4data(P, "goodbye", 3000), ok ),
    erlang:port_close(P).

%%--------------------------------------------------------------------

test_line(P) ->
    ?assert( is_port(P) ),
    ?assertEqual( wait4data(P, {eol, "ok"}, 3000), ok ),
    send(P, ["hello", "\n"]),
    ?assertEqual( wait4data(P, {eol, "hello"}, 3000), ok ),
    erlang:port_close(P).

%%--------------------------------------------------------------------
