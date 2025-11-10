CC=gcc

CCFLAGS=-Wall -Wno-unused-result -fopenmp
ASM_CCFLAGS=$(CCFLAGS) -O3
PLAYER_CCFLAGS=$(CCFLAGS) -Ofast -ffast-math -fno-math-errno

LDFLAGS=-lm

all: asm eplayer
.PHONY: all

clean:
	rm -vf asm eplayer
.PHONY: clean

asm: asm.c
	$(CC) $(ASM_CCFLAGS) -o $@ $< $(LDFLAGS)

eplayer: eplayer.c
	$(CC) $(PLAYER_CCFLAGS) -o $@ $< $(LDFLAGS)
