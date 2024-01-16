PROGRAMS= iarrayTest otpEncodeMain


compile: $(PROGRAMS)

test: compile
	./iarrayTest

clean:
	rm -f *.o $(PROGRAMS)

## debugging option -g
debug:
	make clean
	make CFLAGS=-g LDFLAGS=-g


iarray.o: iarray.h
iarrayTest.o: iarray.h
iarrayTest: iarray.o

otpEncodeMain: readfile.o
otpEncodeMain.o: readfile.h
readfile.o: readfile.h
