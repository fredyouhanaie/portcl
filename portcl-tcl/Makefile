
.SUFFIXES: .beam .erl

.erl.beam:
	erlc $<


.PHONY: all
all: test1.beam


.PHONY: test
test: test1.beam test1.tcl portcl.tcl pkgIndex.tcl
	erl -noshell -s eunit test test1 -s init stop


.PHONY: clean
clean:
	rm -f *.beam erl_crash.dump
