
-module(complex1e_tests).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    complex1:start(["priv/erlport2.tcl"]),
    receive after 500 -> ok end,
    [?assertEqual( 4, complex1:foo(3)),
     ?assertEqual(10, complex1:bar(5))
    ],
    complex1:stop().
