# disable implicit rules
MAKEFLAGS += -rR 

# utilize zig as cross compiler
ZIGCC = zig cc -target x86_64-linux-gnu

test.o: test.c
	$(ZIGCC) -c test.c -o test.o

all: test.o