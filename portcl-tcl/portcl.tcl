
# portcl.tcl --
#
#	This package provides a set of utilities for accepting and
#	processing Erlang port communications.
#
# Copyright (c) 2014 Fred Youhanaie
# See file License for license details.

package provide portcl 0.1.0

namespace eval ::portcl {
	# portmode is a single character defining the communication mode
	#	1: {packet, 1}
	#	2: {packet, 2}
	#	4: {packet, 4}
	#	l: {line, _} - line length not relevant to the tcl side
	#	s: stream
	variable ::portcl::portmode

	# header_size and header_fmt are only used for packet mod
	#
	# header_size is one of 1,2 or 4
	variable ::portcl::header_size

	# header_fmt is one of c, S or I, corresponding to header_size
	# of 1, 2 or 4 respectively
	variable ::portcl::header_fmt
}


# ::portcl::get_mode --
#
#	Extract the port communication mode from the command line args.
#	For now, no application dependent command line args are expected.
#	The expected usage is as follows:
#		script [ -<mode> ] [ -- ] [ <other opts> ]
#	where <mode> is one of 1,2 4, s or l
#	If the application expects any args that can clash with the
#	portcl ones, they should be preceeded by a "--".
#
# Arguments:
#	default_mode	The default portmode to return, if none was
#			given on the command line.
#
# Results:
#	The single character port mode is returned.
#
proc ::portcl::get_mode {default_mode} {

	# handle the trivial case
	if {$::argc == 0} { return $default_mode }

	set opt [lindex $::argv 0]

	if {[regexp {^-[124sl]$} $opt]} {
		set portmode "[string index $opt 1]"
	}

	# shift the arg list, if we have port mode option
	if [info exists portmode] {
		set ::argv [lindex $::argv 1 end]
		set opt [lindex $::argv 0]
	} else {
		set portmode $default_mode
	}

	if {$opt == "--"} {
		set ::argv [lindex $::argv 1 end]
	}

	return $portmode
}


# ::portcl::init --
#
#	portcl::init processes any portcl related command line arguments,
#	and leaves the rest to the main script.
#	By default we expect the erlang side to use {packet, 2} format
#	for communication.
#
# Arguments:
#	defaultmode	(optional) the default mode to pass to get_mode.
#			If none is given to this proc, then {packet, 2}
#			is assumed, which is also the erlang default.
#
# Results:
#	The init function returns 0 on success, and -1 on error.
#
proc ::portcl::init {{defaultmode 2}} {
	if [catch {
		::portcl::set_mode  [::portcl::get_mode $defaultmode]
		} result
	] {
		return -code error $result
	}

	return 0
}


# ::portcl::set_mode
#
#	Set the port comms mode. The proc can be called either manually,
#	or via ::portcl::init, where the mode is obtained from the
#	command line args.
#
# Arguments:
#	portmode	The comms mode to expect from the erlang side
#			- 1, 2 or 4 implies {packet, N}, where N is 1, 2 or 4
#			- s implies stream mode
#			- l implies line mode
#
# Results: none.
#
proc ::portcl::set_mode {portmode} {
	set ::portcl::portmode $portmode
	switch -- $portmode {
		1 {	set ::portcl::header_size 1
			set ::portcl::header_fmt  c
			}
		2 {	set ::portcl::header_size 2
			set ::portcl::header_fmt  S
			}
		4 {	set ::portcl::header_size 4
			set ::portcl::header_fmt  I
			}
	}
	# treat line mode as normal, the rest are special
	if {$::portcl::portmode == "l"} {
		chan configure stdin  -translation auto -buffering line
		chan configure stdout -translation auto -buffering line 
	} elseif {$::portcl::portmode == "s"} {
		chan configure stdin  -translation binary -blocking 0
		chan configure stdout -translation binary -buffering none 
	} else {
		chan configure stdin  -translation binary
		chan configure stdout -translation binary -buffering none 
	}
	return
}


#
# ::portcl::get_header
#	read and return the header bytes from stdin.
#
# Arguments:	none.
#
# Results:
#	length of data in bytes to expect, or -1 if eof encountered
#
proc ::portcl::get_header {} {
	set data [read stdin $::portcl::header_size]
	if [chan eof stdin] {return -1}
	binary scan $data $::portcl::header_fmt len
	return $len
}


#
# ::portcl::send_data
#	send data to the erlang side
#
# Arguments:
#	size	number of bytes in data
#	data	the data to send
#
# Results:
#	whatever puts may return!
#
proc ::portcl::send_data {data} {
	switch -regexp  $::portcl::portmode {
		{[124]} {
			set size [string bytelength $data]
			set msg [binary format ${::portcl::header_fmt}a$size $size $data]
			puts -nonewline stdout $msg
			}
		l {	puts stdout $data
			}
		s {	puts -nonewline stdout $data
			}
	}
}


#
# ::portcl::stdin_event_handler
#	configure event mode operation on stdin
#	(as opposed to own busy-ish poll loop)
#
# Arguments:
#	handler	script to handle the stdin readable event
#
# Results: none.
#
proc ::portcl::stdin_event_handler {handler} {
	chan event stdin readable $handler
}


#
# ::portcl::get_data
#	read and return data, based on portmode
#
# Arguments: none
#
# Results:
#	data if successful
#	error exception if eof, or something goes wrong
proc ::portcl::get_data {} {
	if [regexp {[124]} $::portcl::portmode] {
		set len [::portcl::get_header]
		if {$len < 0} { return -code error eof }
		set data [read stdin $len]
		if [chan eof stdin] { return -code error eof }
		return $data
	} elseif {$::portcl::portmode == "l"} {
		gets stdin data
		if [chan eof stdin] { return -code error eof }
		return $data
	} elseif {$::portcl::portmode == "s"} {
		set data [read stdin]
		if [chan eof stdin] { return -code error eof }
		return $data
	} else {
		return -code error "bad portmode ($::portcl::portmode)"
	}
}

