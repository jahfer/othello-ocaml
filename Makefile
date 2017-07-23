build:
	corebuild src/main.d.byte
clean:
	corebuild -clean
run:
	./main.d.byte
.PHONY: build clean
