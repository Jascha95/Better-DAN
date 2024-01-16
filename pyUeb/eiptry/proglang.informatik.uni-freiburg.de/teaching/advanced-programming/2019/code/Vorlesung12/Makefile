PROGRAMS= globalBufferOverflow heapBufferOverflow stackBufferOverflow useAfterFree memoryLeak

CFLAGS= -g -fsanitize=address
LDFLAGS= -g -fsanitize=address

compile: $(PROGRAMS)

clean:
	rm -f $(PROGRAMS)
	rm -rf *.dSYM

