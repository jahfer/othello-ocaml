build:
	corebuild src/main.d.byte
clean:
	corebuild -clean
run:
	./main.d.byte
test:
	corebuild test/compose_test.d.byte && ./compose_test.d.byte
.PHONY: build clean
