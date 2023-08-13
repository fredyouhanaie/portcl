#!/usr/bin/env tclsh

# erl_comm.tcl
#	This is the Tcl equivalent of the C ports example found in the
#	OTP System Documentation tutorial.

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

while true {
    # get the header
    set hdr [read stdin $::header_size]
    if [chan eof stdin] { got_eof }
    binary scan $hdr $::header_fmt len

    # get the commad and data
    set data [read stdin $len]
    if [chan eof stdin] { got_eof }
    datadump $data
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
