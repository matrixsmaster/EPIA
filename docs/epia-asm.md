# Quick assembler reference

>To be expanded

Notes about EPIA assembly:

1. it's line-oriented - every instruction/definition has to be on a single line, and occupy only that line
1. it's tokenized by space, tab, and comma - therefore those symbols will be lost unless encapsulated into a double-quoted string

## Code emission control

* `.addr` sets the current virtual IP to `addr`
* `.+offset` moves the current virtual IP by `offset`
* `.?align` aligns the IP to the nearest (incrementing) address divisible by `align`

The argument for '.' controller could be either an immediate value, or a label/macro (but not an expression!)

### Examples:

```
.0 ;start emitting code at address 0 (beginning of RAM)

.+1024 ;advance the current address by 1024 bytes

.?8 ;align current address to the next 8-byte boundary
```

## Arguments

1. A direct numeric value means itself, and will be used as-is in its context
1. A label or symbolic name will be substituted by the address of that label or named data field
1. A macro will be substituted by its value
1. An expression will be computed and substituted by the result. However, inlined expressions (used in the code, not in a macro) will be computed at the end of the translation cycle, meaning they _could_ reference names defined later in the program (postponed expansion)

### Modifiers

* `%` means an immediate value - it will set 'immediate value' flag of the respective field inside the instruction code, signifying that corresponding argument doesn't need to be dereferenced
* `@` means a pointer value - it will set 'pointer' flag of the respective field inside the instruction code, signifying that corresponding argument has to be dereferenced twice

#### Examples:

```
mov a %10 ;put value '10' into location named 'a'
dd a 0 ;create the location 'a', and set its initial value to 0
```

```
mov ret_addr %label ;store label's address into location 'ret_addr'
jmp @ret_addr ;jumps to the address stored inside location 'ret_addr'
movb @dst @src ;similar to C's `*(char*)dst = *(char*)src`
```

## Macros

Format: "#NAME Value"

`Value` could be either another macro name, label, numeric value, or an expression. If it's an expression, it will be calculated immediately (i.e., requiring all its arguments to be known at that point).

### Examples:

```
#A 5
#B A
```

## Expressions

Since the current assembler pre-tokenizing lines by whitespace, it's prohibited inside expressions.

Expressions could contain other expressions, references to macros, symbolic names, labels, numeric values.

### Examples:

```
#A 5
#B 10
#C !(A*B+0x10) ; C will be computed as 66
#D !((A+1)*B) ; D will be computed as 60
```

```
cmp a b
jif %!(mylabel+8) %1 ; will jump past the NOP if a == b
:mylabel
nop
```

## Namespaces

Namespaces are used to mark certain parts of the code as belonging to a certain space (and thus allowing to re-use names).

### Examples:

```
>myspace
db mybyte 0
:mylabel
<

>other_space
db mybyte 0 ;now it's a different byte compared to 'myspace.mybyte'
:mylabel ;new label, same name
<
```

It's extremely useful to create function-local storages:

```
:foo ;mark beginning of foo(), in global namespace
>foo ;start foo()'s local namespace
    ; some code here
    :loop ;typical label name used in many other functions
    ...
    :endloop

dd i 0 ; typical variable name used in many other functions
< ;end foo()'s local namespace
```

## Special markers

For now, the assembler supports only one special marker - `STOP`. If it sees that marker at the beginning of a line, it stops reading the input file and starts emitting code.

## Copyright

(C) Dmitry 'sciloaf' Solovyev, 2005

(C) Syntheva AS, 2025
