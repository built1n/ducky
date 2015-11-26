CC = gcc
OUT = build
PLATFORM = unix

DUCKY_OBJ = src/interp.o src/compile.o src/vm.o src/emitc.o src/common.o

CFLAGS = -lm -Og -g -I src/ -I target/$(PLATFORM)

all: $(OUT)/$(PLATFORM).bin

$(OUT)/$(PLATFORM).bin: $(DUCKY_OBJ) target/$(PLATFORM)/main.o Makefile
	mkdir -p $(OUT)
	$(CC) $(DUCKY_OBJ) target/$(PLATFORM)/main.o $(CFLAGS) -o $(OUT)/$(PLATFORM).bin

install: $(OUT)/$(PLATFORM).bin
	install $(OUT)/$(PLATFORM).bin /bin/ducky

clean:
	rm -f $(OUT)/$(PLATFORM).bin
	rm -f target/$(PLATFORM)/main.o
	rm -f $(DUCKY_OBJ)
