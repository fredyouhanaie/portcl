#!/usr/bin/env tclsh

# test1.tcl
#	basic test of connect/receive/reply

package require portcl

proc diag {msg} {
	tclLog "$::argv0: $msg"
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
		} else {
			set ::forever 1
			return
		}
	}
	::portcl::send_data [string bytelength $data] $data
}

::portcl::init 1

::portcl::stdin_event_handler getdata

::portcl::send_data 2 ok

diag "ready to rock and roll (portmode=$::portcl::portmode)"

vwait forever
