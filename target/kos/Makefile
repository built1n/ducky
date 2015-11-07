include .knightos/variables.make

#INCLUDE+=add to your include path

HEADERS:=$(wildcard *.h)

ALL_TARGETS:=$(BIN)ducky

$(BIN)ducky: $(OUT)main.o $(OUT)crt0.o $(OUT)../../src/ducky.o
	mkdir -p $(BIN)
	scas $(ASFLAGS) $(OUT)crt0.o $(LIBRARIES) $(OUT)main.o $(OUT)../../src/ducky.o -o $(BIN)ducky

include .knightos/sdk.make
