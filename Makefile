
all:
	go build
	for i in examples/*.vim; do ./yorda $$i >$$i.pl; done

test:
	swipl -s lib/test.pl

repl:
	cd lib && swipl -s vimscript.pl

.PHONY: all test repl
