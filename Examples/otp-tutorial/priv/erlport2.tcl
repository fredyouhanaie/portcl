#!/usr/bin/env tclsh

# erlport2.tcl
#	This is the event driven version of erlport.tcl, which uses
#	the portcl package.

package require portcl

set ::header_size 2	;# should match that on the erlang side
set ::header_fmt S	;# big-endian short

proc diag {msg} {
    puts stderr "$::argv0: $msg"
}

proc got_eof {} {
    diag "got eof"
    exit
}

# dump the data bytes as a sequence of decimals
proc datadump {data} {
    binary scan $data c* dd
}

proc foo {X} { return [expr $X + 1] }
proc bar {Y} { return [expr $Y * 2] }

chan configure stdin  -translation binary
chan configure stdout -translation binary -buffering none 

proc get_data {} {
    diag "got some data"
    if [ catch ::portcl::get_data data ] {
        diag "caught exception, data=$data."
        if {$data == "eof"} {
            got_eof
            return
        } elseif {$data == "nodata"} {
            # data not ready
            return
        } else {
            diag $data
            set ::forever 1
            return
        }
    }

    # we expect 2 bytes, command and arg
    binary scan $data cc cmd arg
    set cmd [expr $cmd & 0xff]
    set arg [expr $arg & 0xff]

    # execute the command
    switch -- $cmd {
        1	{set res [foo $arg]}
        2	{set res [bar $arg]}
        *	{set res -1}
    }

    # encode and return the result, length and result
    set data [binary format ${::header_fmt}c 1 [expr $res & 0xff]]
    datadump $data
    puts -nonewline stdout $data
}

::portcl::init
diag "init completed, portmode=$::portcl::portmode"

::portcl::stdin_event_handler get_data

vwait forever
