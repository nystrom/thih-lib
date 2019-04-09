build:
	stack build

clean:
	stack clean

run:
	stack run

test: FORCE
	stack test

FORCE:
