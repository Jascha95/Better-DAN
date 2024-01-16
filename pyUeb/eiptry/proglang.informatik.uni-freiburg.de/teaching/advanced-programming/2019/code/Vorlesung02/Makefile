all: compile test

clean:
	rm -f histogramMain

compile: histogramMain

test: histogramMain
	echo AAAAAAAAAA | ./histogramMain
	echo AAABBBCCC | ./histogramMain
	./histogramMain < histogramMain.c

# which object files are needed?
histogramMain: histogramMain.o histogram.o

# necessary to document the use of the include file
histogram.o: histogram.h
histogramMain.o: histogram.h

