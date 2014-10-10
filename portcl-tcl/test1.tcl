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
	if [regexp {[124]} $::portcl::portmode] {
		getdata_124
	} elseif {$::portcl::portmode == "l"} {
		getdata_l
	} elseif {$::portcl::portmode == "s"} {
		diag "no stream test yet - ignored!"
	} else {
		diag "bad portmode ($::portcl::portmode)"
		set ::forever 1
	}
}

proc getdata_124 {} {
	diag "reading header"
	set len [::portcl::get_header]
	diag "len=$len."
	if {$len < 0} { got_eof; return }
	diag "reading data"
	set data [read stdin $len]
	if [chan eof stdin] { got_eof; return }
	diag "data=$data."
	::portcl::send_data [string bytelength $data] $data
}

proc getdata_l {} {
	diag "reading data"
	gets stdin data
	if [chan eof stdin] { got_eof; return }
	diag "data=$data."
	::portcl::send_data [string bytelength $data] $data
}

::portcl::init 1

::portcl::stdin_event_handler getdata

::portcl::send_data 2 ok

diag "ready to rock and roll (portmode=$::portcl::portmode)"

vwait forever
