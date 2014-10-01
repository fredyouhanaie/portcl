
# portcl

`portcl` helps one create Erlang ports based on Tcl scripts.

This project contains two libraries with convenience functions, and a
bunch of example applications.

Both Tcl and Erlang are mature scripting languages that were conceived
and created in the mid to late 80s, and are still being developed.

A port (program/script) written in Tcl will act as a subordinate process
to the Erlang process/node (VM) that launched it. A companion project,
`etclface`, enable one to create an independed Tcl based process that
will look like another Erlang process.
