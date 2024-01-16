PROGRAMS= json_test json_read_test
OBJECTS= *.o $(PROGRAMS)

compile: $(PROGRAMS)

test: compile
	./json_test
	./json_test ex1.string
	./json_test ex1.number
	./json_test ex1.json
	./json_test ex2.json
	./json_test ex3.json
	./json_test ex4.json
	./json_test Makefile
	# ./json_read_test ex1.json
	# ./json_read_test ex2.json
	# ./json_read_test ex3.json
	# ./json_read_test Makefile

clean:
	rm -f $(OBJECTS)

checkstyle:
	python ../../cpplint.py --repository=. --filter=-runtime/int,-readability/casting,-runtime/threadsafe_fn,-build/include_subdir *.c *.h

debug:
	make clean
	make compile CFLAGS=-g LDFLAGS=-g

json_parser.o: json_parser.h
json_test.o: json_parser.h
json_test: json_parser.o

json_read_test: json_reader.o json_data.o json_parser.o
json_read_test.o: json_reader.h json_data.h
json_data.o: json_data.h
json_reader.o: json_data.h json_parser.h json_reader.h
