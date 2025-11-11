# EPIA - Instruction Set Reference

> This file lists all EPIA instructions, their arguments, width behavior, effects and side-effects. For the architectural overview, please look at [epia-arch.md](epia-arch.md)

---

## Conventions

- EPIA is little-endian 64-bit architecture
- Instruction form: `<MNEMONIC> [destination] [source1] [source2]`.
- Width-modifier suffixes are appended to the mnemonic: `b` (byte, 8-bit), `h` (halfword, 16-bit), `w` (word, 32-bit), `d` (doubleword, 64-bit - default).
  - Examples: `movb dest src` (copy a byte), `cmpw a b` (compare 32-bit words).
- Widths are semantically important. Using the wrong width can silently truncate or change results. Examples:
  - Truncation: `movb ptr src` will copy only the low 8 bits of a 64-bit pointer and thus corrupt addresses.
  - Expansion: `add r a b` while `a` and/or `b` are lesser than 64-bit wide will produce incorrect result as both `a` and `b` will be interpreted as 64-bit integers (and read from memory accordingly).
  - Arithmetic differences: `addb A B C` adds only the low 8 bits; `addw A B C` uses 32-bit lanes - carries/overflow are different.
- Some instructions *change* the effective width or have implicit width behavior:
  - `fti` (float to int) **reads** a 32-bit float and **writes** an integer using the width specified by the instruction suffix (e.g. `ftiw` writes a 32-bit integer, `ftid` writes a 64-bit integer).
  - `itf` (int to float) **reads** an integer of the indicated width and **converts** it to a 32-bit float (float size is fixed to 32-bit in this ISA).
- Because EPIA has no R/W CPU registers, every operand refers to memory (operated via cache in HW).

### Flag register (read-only for programs)

Bit layout (7 .. 0):

```
7 6 5 4 3 2 1 0
Z x x x x L G E
```

- `Z` - Zero flag: set if an instruction wrote `0` into its destination (cleared otherwise). Arithmetic-style writes update `Z`.
- `L` - Lesser-than flag (set by `cmp` family when left < right).
- `G` - Greater-than flag (set by `cmp` family when left > right).
- `E` - Equal flag (set by `cmp` family when left == right).
- `x` - reserved / unused.

---

## Instruction table

| Instruction | Args | Effect | Flags / Exceptions | Notes / Example |
|---|---|---|---|---|
| `nop` | - | No operation. | - | |
| `hlt` | - | Halt execution | - | |
| `mov` | `dest src` | `dest = src` (copy integer bits) | - | `movw dst src` - copy 32-bit word |
| `cmp` | `a b` | Compare unsigned `a` and `b` | sets `L/G/E` | `cmp a b` |
| `cms` | `a b` | Compare signed `a` and `b` | sets `L/G/E` | |
| `cmf` | `a b` | Compare floats `a` and `b` | sets `L/G/E` | |
| `add` | `dest a b` | `dest = a + b` (integer addition, unsigned arithmetic view) | updates `Z` | `addw R A B` |
| `adf` | `dest a b` | `dest = a + b` (float add) | updates `Z` (float-zero check) | `adf Fout Fa Fb` |
| `sub` | `dest a b` | `dest = a - b` (signed integer subtraction) | updates `Z` | For unsigned subtraction use `suu` |
| `suu` | `dest a b` | `dest = a - b` (unsigned subtraction) | updates `Z` | |
| `suf` | `dest a b` | `dest = a - b` (float subtraction) | updates `Z` | |
| `neg` | `dest src` | `dest = -src` (signed negate) | updates `Z` | |
| `ngf` | `dest src` | `dest = -src` (float negate) | updates `Z` | |
| `sex` | `dest src new_width` | Sign-extend `src` into `dest` (write according to `new_width`) | updates `Z` | `sexb dst val %4` - sign-extend 8-bit `val` into 32-bit destination |
| `and` | `dest a b` | `dest = a & b` (bitwise AND) | updates `Z` | |
| `ior` | `dest a b` | `dest = a \| b` (bitwise OR) | updates `Z` | |
| `xor` | `dest a b` | `dest = a ^ b` (bitwise XOR) | updates `Z` | |
| `shl` | `dest a b` | `dest = a << b` (logical left shift) | updates `Z` | |
| `shr` | `dest a b` | `dest = a >> b` (logical right shift) | updates `Z` | |
| `not` | `dest src` | `dest = ~src` (bitwise NOT) | updates `Z` | |
| `mul` | `dest a b` | `dest = a * b` (integer multiply, unsigned view) | updates `Z` | Beware overflow semantics (truncation to width) |
| `muf` | `dest a b` | `dest = a * b` (float multiply) | updates `Z` | |
| `div` | `dest a b` | `dest = a / b` (signed integer division) | updates `Z`, raises CPU exception on zero divisor | |
| `diu` | `dest a b` | `dest = a / b` (unsigned integer division) | updates `Z`, raises CPU exception on zero divisor | |
| `mod` | `dest a b` | `dest = a % b` (unsigned modulus) | updates `Z`, raises CPU exception on zero divisor | |
| `dif` | `dest a b` | `dest = a / b` (float division) | updates `Z`, raises CPU exception on zero divisor | |
| `ivf` | `dest src` | `dest = 1.0f / src` (float reciprocal) | raises CPU exception on zero divisor | |
| `abf` | `dest src` | `dest = fabs(src)` (float absolute value) | updates `Z` | |
| `sqr` | `dest src` | `dest = sqrt(src)` (float square root) | updates `Z` | |
| `exp` | `dest src` | `dest = exp(src)` (float exponential) | updates `Z` | |
| `sin` | `dest src` | `dest = sin(src)` (float sine) | updates `Z` | |
| `cos` | `dest src` | `dest = cos(src)` (float cosine) | updates `Z` | |
| `rnf` | `dest src` | `dest = round(src)` (float round to nearest) | updates `Z` | |
| `pow` | `dest a b` | `dest = pow(a, b)` (float power) | updates `Z` | |
| `itf` | `dest src` | `dest = (float)src` - integer-to-float conversion. *Reads* integer of the indicated width, *writes* a 32-bit float | updates `Z` | |
| `fti` | `dest src` | `dest = (int)src` - float-to-integer conversion. *Reads* a 32-bit float and *writes* an integer using the instruction suffix width (e.g. `ftiw` -> 32-bit int, `fti` -> 64-bit int) | updates `Z` | |
| `sip` | `dest` | `dest = IP` (store instruction pointer) | - | |
| `sfl` | `dest` | `dest = FLAGS` (store flags byte) | - | |
| `jif` | `target mask` | jump if `FLAGS & mask` are non-zero (conditional jump) | - | mask tests the flag byte bits (Z/L/G/E) |
| `jmp` | `target` | unconditional jump to target | - | `jmp label` |
| `ppu` | `ID_loc NThreads Barrier` | Start the PPU with `NThreads` worker threads. Stop at `Barrier` address | - | For each spawned thread a sequential ID (starting from 0) is written to `ID_loc` (that address is *thread-local*).  |
| `clo` | `dest` | Mark `dest` as clobbered (*thread-local*), preventing cross-contamination of same memory areas referenced by different threads | - | Use inside parallel code fragments to mark thread-local locations: `clo addr` |
| `dpu` | `dest` | Start the SDPU and return its scalar result | updates `Z` | |
| `dpr` | `dest index` | `dest = DPU_REGS[index]` (read DPU register) | updates `Z` | |
| `dpw` | `index value` | `DPU_REGS[index] = value` (write DPU register) | - | |
| `out` | `value port` | `IO[port] = value` (write to I/O port) | - | mind the order |
| `inp` | `dest port` | `dest = IO[port]` (read from I/O port) | updates `Z` | |

---

## Notes & implementation details

- **Zero flag (`Z`) update rule**: Many arithmetic/data instructions update `Z` as they write to the destination. Integer writes test whether `dest == 0` (unsigned view); floating writes test whether `dest == 0.0f`.
- **Sign-extension control**: any instruction expecting signed values will sign-extend the arguments prior to operation, according to the width field set.
- **Floating vs integer ops**: Float instructions operate on 32-bit IEEE floats (single precision). Conversions between integer and float are explicit (`itf`, `fti`).
- **Clobbered locations**: Locations marked as clobbered allowing the PPU-aware code to execute identical code parts without worrying about using same intermediate locations (iterators, temporary results, etc). In HW this is implemented via a cache lock (cache won't synchronize locations marked as clobbers, but also won't overwrite them with other data). The maximum number of locations which could be marked as clobbered is implementation-dependent and currently is fixed at 32 in EPIA emulators.

## Copyright

(C) Dmitry 'sciloaf' Solovyev, 2005

(C) Syntheva AS, 2025
