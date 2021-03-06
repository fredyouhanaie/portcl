
# portcl/Examples

This directory contains various small examples for playing and/or
experimenting with `portcl`.

---

## Buttons

`buttons` is an example of a simple Tcl/Tk based control panel that sends
messages to its Erlang (parent) process. The erlang process waits for
messages and prints them on the standard output.

Running `make test` will compile and run the erlang buttons process. Note
that you will get two identical windows, each with its own set of buttons.

The Tcl script can also be run on its own, in which case it will send
its messages to the standard output, i.e.:

> env TCLLIBPATH=../portcl-tcl ./buttons.tcl 

---

More examples will be added here.

