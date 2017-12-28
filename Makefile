clean:
	jbuilder clean

test:
	jbuilder runtest

js:
	bsb -make-world

.PHONY: clean test js
