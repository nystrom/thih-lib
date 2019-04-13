build:
	stack build --fast

clean:
	stack clean

run: build
	stack run

test: build
	stack test --fast
