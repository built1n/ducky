CC = gcc
PLATFORM = unix

DUCKY_OBJ = src/ducky.o

CFLAGS = -lbsd -lm -O2 -g -I src/ -I target/$(PLATFORM)

$(PLATFORM).bin: $(DUCKY_OBJ) target/$(PLATFORM)/main.o
	$(CC) $(DUCKY_OBJ) target/$(PLATFORM)/main.o $(CFLAGS) -o $(PLATFORM).bin

clean:
	rm -f $(PLATFORM).bin
	rm -f target/$(PLATFORM)/main.o
	rm -f $(DUCKY_OBJ)
