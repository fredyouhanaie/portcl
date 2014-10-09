
.SUFFIXES: .beam .erl

.erl.beam:
	erlc $<

PORTCLLIB=-env TCLLIBPATH ../portcl-tcl

BEAMS=$(patsubst %.erl, %.beam, $(wildcard *.erl))

.PHONY: all
all: ${BEAMS}


.PHONY: test
test: buttons.beam buttons.tcl
	erl $(PORTCLLIB) -noshell -s buttons -s init stop

.PHONY: clean
clean:
	rm -f *.beam erl_crash.dump

