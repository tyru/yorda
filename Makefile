
all:
	go build
	bash -c 'for i in examples/*.vim; do ./yorda $$i >$${i:0:-3}pl; done'

test: all
	for i in examples/*.pl; do swipl -s $$i || echo "error: '$$i' exited with $$?"; done
	swipl -s lib/test.pl

repl:
	cd lib && swipl -s vimscript.pl

.PHONY: all test repl
