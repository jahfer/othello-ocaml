clean:
	corebuild -clean

build:
	corebuild src/main.byte
run:
	./main.byte

build-test:
	corebuild -pkg oUnit,str -Is src test/test.byte
test:
	make build-test && ./test.byte

.PHONY: build run clean test build-test
