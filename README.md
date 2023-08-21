
# portcl

`portcl` helps one create Erlang ports based on Tcl scripts.

__Note:__ The source code license has been changed to Apache 2.0. This
is in order to be consistent with my other Erlang based projects.

## Introduction

An Erlang port is a non-Erlang external program (or linked driver)
that appears to the Erlang processes as another process and can
communicate with these processes via the message passing mechanisms.

This project is about ports written in Tcl. Such a script will act as
a subordinate process to the Erlang process/node (VM) that launched
it, and so it can only last as long as the Erlang owner process is
alive.

For an independent/standalone Tcl script that can communicate with
Erlang processes please see the companion project -
[etclface](https://github.com/fredyouhanaie/etclface).

## Packages and Modules

This project contains two libraries with convenience functions, and a
bunch of example applications.

As can be seen from the `Examples/otp-tutorial/` example, a Tcl based
port can be written without the need for libraries, however, the
purpose of these is to avoid repetitive cut-n-paste of code.

The aim of the project is to expand the modules for various patterns,
such as gui dashboards, rpc calls etc.

## Feedback and contributions

All feedback and contributions are welcome. Please use the issue
tracker and pull requests for bugs and code contributions.


Enjoy

Fred Youhanaie
