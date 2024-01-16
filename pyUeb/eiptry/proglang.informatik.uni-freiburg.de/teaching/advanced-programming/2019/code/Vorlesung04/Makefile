CC := gcc -Wall -pedantic -std=c11
PROGRAMS = concatMain

all: compile test checkstyle

compile: $(PROGRAMS)

test: concatMain
	./concatMain a ab abc ""

checkstyle:
	python3 ../cpplint.py --repository=. --filter=-runtime/int,-readability/casting,-runtime/threadsafe_fn,-build/include_subdir *.h *.c

clean:
	rm -f *.o $(PROGRAMS)

## dependencies
concatMain: concatMain.o concat.o
concatMain.o: concat.h
concatMain.o: concat.h
