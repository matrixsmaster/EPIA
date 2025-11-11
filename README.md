# EPIA - Easily Parallel Instruction Architecture

EPIA (Easily Parallel Instruction Architecture) is an open-source NPU architecture designed for efficient on-device inference and fine-tuning of medium-sized LLMs and VLMs.
It targets predictable, low-latency, power-efficient hardware implementations while remaining easy to program - even at assembly level.

---

## Key features

- **Massive parallelism** - EPIA NPUs contain a large amount of cores to help executing parallel code as fast as possible.
- **Native 64-bit pipeline** - data, commands and addresses are all 64-bit wide.
- **Flexible, efficient FPU** - a rich set of fp32 FPU instructions (fp16 compatibility planned).
- **Simple and intuitive programming model** - clear code flow and assembly-level friendliness.
- **Lean, high-performance design** - smaller and less complex than many modern stacks while delivering comparable capability.
- **Orthogonal instruction set and addressing modes** for expressive yet predictable programs.
- **Hardware-level parallelization model** - obvious, predictable task partitioning between cores.
- **Transparent large-tensor handling** - no explicit sharding required.
- **No manual affinity management** - no sticky nodes or manual affinity juggling required for sequential code sections.
- **Reduced synchronization complexity** - hardware synchronization relieves programmers of most manual sync duties.
- **Zero active-wait states** - cores stop at barriers and restart on command within one clock cycle.
- **Power efficiency** - sequential parts of the code will be executed by a single core while the PPU is halted, lowering power consumption, memory bandwidth used, and number of IO operations.
- **Hardware-accelerated vector dot-product** with flexible per-block scaling to support multiple tensor quantization schemes in hardware.

---

## Documentation

- [Architecture overview](docs/epia-arch.md)
- [Complete ISA](docs/epia-isa.md)
- [Assembler syntax](docs/epia-asm.md)
- [Full article](https://robotics.syntheva.no/articles/epia)

---

## Quickstart

### Building

From the repository root:

- Build the assembler/emulator:
```
make asm
```

- Build EPlayer (stand-alone emulator):
```
make eplayer
```

### Example: EPIA Assembler / Emulator

The assembler/emulator supports common development-time options:

```
Usage: <prog> [OPTIONS]
  -i <input_assembly.asm>         Input assembly file
  -o <output_binary.bin>          Output binary
  -m <memory_size_in_bytes>       Allocate output memory buffer
  -e                              Execute binary after translation
  -d                              Dump binary as hex to stdout
  -M <addr>:<file>                Memory-map a file starting at <addr>
  -W                              Allow write access to memory-mapped file
  -T                              Enable execution timing
  -l                              List macros and their computed values
  -u                              List unused identifiers
  -c                              Cut output at last IP value
  -S                              Stop at first error
```

Examples:
```
# assemble and write output
./asm -i demo/hello.asm -o hello.bin

# assemble, print a short hex dump, then execute
./asm -i demo/hello.asm -d -c -e

# simply assemble and execute with all default parameters
./asm -i demo/maze.asm -e

# set output buffer size (decimal or hex)
./asm -i demo/hello.asm -o hello.bin -m 65536

# assemble, memory-map a file and execute large code
./asm -i demo/mistral.asm -m 0x80000000 -M 0x80000000:mistral-0.3.bin
```

> Notes:
> - Use hex addresses such as `0x10000000` where appropriate.
> - `-M` expects `addr:file` with a valid file path.

### Example: EPlayer (stand-alone emulator)

Minimal invocation:

```
Usage: eplayer <executable> [RAMsize] [<mapping_address> <mapping_file>]
```

Example runs:
```
# run executable with default RAM
./eplayer hello.bin

# run with explicit RAM size
./eplayer hello.bin 0x10000

# compile, then run with a memory-mapped file
./asm -i demo/mistral.asm -m 0x80000000 -c -o mistral.bin
./eplayer mistral.bin 0x80000000 0x80000000 mistral-0.3.bin
```

- If RAM size is omitted, `DEFAULT_MEMORY` (configured in source) is used.
- When mapping is requested, use `<mapping_address>` and `<mapping_file>` as shown (address can be hex). `<mapping_address>` refers to internal (EPIA) memory.

#### Notes:

* Performance: the EPlayer emulator is highly optimized - on a modern Core i9 @ 4.1 GHz it can emulate EPIA NPU running LLaMA-2 compatible 7B model at ~750 ms/token (measured on host machine).
* Special data streams: for debugging purposes, EPIA emulators support the following special IO ports:
  - IOCHAR (0) - reads or writes a single byte character to the terminal
  - IOUINT (1) - reads or writes an unsigned integer as text
  - IOINT (2) - same but with signed integer
  - IOFLOAT (3) - same but with a floating-point value
  - IOSTRING (4) - writes a zero-terminated string to the terminal
  - IOPOINTER (5) - writes a 64-bit pointer (hex string) to the terminal

### DEMOS

There are a couple of quite sophisticated demos included in this project repo.

#### The Maze

This demo shows a 3D maze rendered in ASCII art right in your terminal. The walls, floors, and ceilings are all textured. The demo is interactive - you can use WASD to navigate around the maze in realtime!

To run, simply launch
```
scripts/maze.sh
```

**Notes**:

1. The demo is using non-canonical terimnal mode, so some leftovers of your current terminal contents might be visible around the 3D viewport during the play.
1. The default resolution is 180x60 characters. If it's too large (or too small) for you, please change `SCREENW` and `SCREENH` macros in `demo/maze.asm` before running.


#### LLaMA-2 model inference (tested with Mistral 0.3)

This demo shows the real power of EPIA NPUs - LLM inference. The demo requires a LLaMA-2 compatible model in GGUF format with Q8_0 quantization.

Once you obtained the model file (I recommend using `Mistral-7B-Instruct-v0.3-Q8_0.gguf` which you can easily find on HuggingFace), run the demo:
```
scripts/mistral.sh Mistral-7B-Instruct-v0.3-Q8_0.gguf
```

**Notes**:

1. The demo is resource-heavy (as expected for an emulation of a hardware running a multi-billion weights LLM).
1. At the first run, the script will create "The Blob" - a representation of the source model made for EPIA NPU.
1. The demo has only a greedy sampler for now, so the generated text might look repetitive after certain length.
1. The demo has no conversions for special characters / UTF sequences yet, so they will be used verbatim.


---

## Development & contribution

Contributions, bug reports and feature requests are welcome. Please:

1. Open an issue describing the problem or feature.
2. If submitting code, follow the repo's style and include tests or examples where appropriate.
3. Keep commits small and focused; include a clear description.

If you want help with integration, FPGA targets, or toolchain patches I can assist with review and suggestions.

---

## License & copyright

(C) Dmitry 'sciloaf' Solovyev, 2025

(C) Syntheva AS, 2025

EPIA is released under **GPL v2**.

---

## Links

- [syntheva.no](https://syntheva.no)
