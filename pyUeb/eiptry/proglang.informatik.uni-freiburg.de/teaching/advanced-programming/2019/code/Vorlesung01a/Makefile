all: compile test

clean:
	rm -f approximationOfPiMain approximationOfPiTest

compile: approximationOfPiMain

test: approximationOfPiTest

# which object files are needed?
approximationOfPiMain: approximationOfPiMain.o approximationOfPi.o

approximationOfPiTest: approximationOfPiTest.o approximationOfPi.o

# necessary to document the use of the include file
approximationOfPi.o: approximationOfPi.h
approximationOfPiMain.o: approximationOfPi.h
approximationOfPiTest.o: approximationOfPi.h
