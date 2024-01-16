PROGRAMS= optionsMain optionsTest
OBJECTS= *.o $(PROGRAMS)

compile: $(PROGRAMS)

test: compile
	./optionsMain
	./optionsMain -g
	./optionsMain --debug --library m -O4
	./optionsTest

clean:
	rm -f $(OBJECTS)

checkstyle:
	python ../../cpplint.py --repository=. --filter=-runtime/int,-readability/casting,-runtime/threadsafe_fn,-build/include_subdir *.c *.h

debug:
	make clean
	make compile CFLAGS=-g LDFLAGS=-g

options.o: options.h
optionsMain.o: options.h
optionsMain: options.o
optionsTest: options.o

