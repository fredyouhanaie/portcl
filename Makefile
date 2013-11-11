
.SUFFIXES: .beam .erl

.erl.beam:
	erlc $<

all:

clean:
	rm -f *.beam erl_crash.dump
