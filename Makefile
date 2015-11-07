CC = gcc

DUCKY_OBJ = src/ducky.o

CFLAGS = -lbsd -lm -O2 -g -I src/

unix: $(DUCKY_OBJ) sysdep/unix.o
	$(CC) $(DUCKY_OBJ) sysdep/unix.o $(CFLAGS) -o unix.bin
