
.SUFFIXES: .beam .erl

.erl.beam:
	erlc $<

all: complex1.beam complex1_tests.beam

clean:
	rm -f *.beam erl_crash.dump
