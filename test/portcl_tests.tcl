#!/usr/bin/env tclsh

# test1.tcl
#	basic test of connect/receive/reply

package require portcl

tclLog "tcl_interactive=$::tcl_interactive"

proc diag {msg} {
    if { $::tcl_interactive == 1 } {
        tclLog "$::argv0: $msg"
    }
}

proc got_eof {} {
    diag "got eof"
    set ::forever 1
}

proc getdata {} {
    diag "reading data"
    if [ catch ::portcl::get_data data ] {
        if {$data == "eof"} {
            got_eof
            return
        } elseif {$data == "nodata"} {
            return
        } else {
            set ::forever 1
            return
        }
    }
    ::portcl::send_data $data
}

::portcl::init 1

::portcl::stdin_event_handler getdata

::portcl::send_data ok

diag "ready to rock and roll (portmode=$::portcl::portmode)"

vwait forever
