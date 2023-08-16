
# OTP Interoperability Tutorial - Ports

This subdirectory contains the example programs found in section 8.4 of
the OTP System Documentation.

The main exception is that the C program in the example has been replaced
with a Tcl script.

## Buld and Run

Use the usual `rebar3` commands:

    $ rebar3 dialyzer

    $ rebar3 eunit

    $ rebar3 shell
    1> complex1:start().
    2> complex1:foo(4).
    5
    3> complex1:bar(4).
    8
    4> complex1:stop().

---
