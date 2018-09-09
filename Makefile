
all:
	go build
	bash -c 'for i in examples/*.vim; do ./yorda $$i >$${i:0:-3}pl; done'

test:
	swipl -s lib/test.pl

repl:
	cd lib && swipl -s vimscript.pl

.PHONY: all test repl
