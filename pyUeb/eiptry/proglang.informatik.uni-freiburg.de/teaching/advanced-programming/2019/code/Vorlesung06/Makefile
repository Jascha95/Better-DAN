PROGRAMS= iarrayTest

## debugging option -g

CFLAGS= -g
LDFLAGS= -g

compile: $(PROGRAMS)

test: compile
	./iarrayTest

clean:
	rm -f *.o $(PROGRAMS)

iarray.o: iarray.h
iarrayTest.o: iarray.h
iarrayTest: iarray.o
