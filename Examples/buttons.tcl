#!/usr/bin/env tclsh

# buttons.tcl
#	tcl/tk buttons that send predefined messages to the erlang port.
#

package require Tk
package require portcl

::portcl::init

::portcl::stdin_event_handler ::portcl::get_data

# create a bunch of Greek buttons on a single row in a frame
ttk::frame .bframe
set col 0
foreach bname {Alpha Beta Gamma Delta} {
	set button .bframe.[string tolower $bname]
	ttk::button $button -text $bname -command "::portcl::send_data [string bytelength $bname] $bname"
	grid $button -row 0 -column [incr col]
}

# an exit button in its own frame
ttk::frame .eframe
ttk::button .eframe.exit -text EXIT -command exit
grid .eframe.exit

# stack the two frames on top of each other
pack .bframe .eframe

