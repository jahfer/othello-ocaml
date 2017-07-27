clean:
	corebuild -clean

build:
	corebuild src/main.byte
run:
	./main.byte

build-test:
	corebuild -pkg oUnit -Is src test/main.byte
test:
	make build-test && ./main.byte

.PHONY: build run clean test build-test
