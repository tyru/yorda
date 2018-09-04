
all:
	go build
	for i in examples/*.vim; do ./yorda $$i >$$i.pl; done

test:
	swipl -s lib/test.pl

.PHONY: all test
