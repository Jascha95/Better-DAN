PROGRAMS= experimentMain

compile: $(PROGRAMS)

test: compile
	./experimentMain 1 999 4711
	./experimentMain 2 999 4711

checkstyle:

clean:
	rm -f *.o $(PROGRAMS)

debug:
	make clean
	make compile CFLAGS=-g LDFLAGS=-g

experiment2.o: experiment2.h
experiment1.o: experiment1.h
experimentMain.o: experiment1.h experiment2.h
experimentMain: experiment1.o experiment2.o
