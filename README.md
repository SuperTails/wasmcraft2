# Wasmcraft

Have you ever wanted to run C/C++/Rust/WebAssembly applications in Minecraft? No? Well, now you can!

Wasmcraft is a compiler from [WebAssembly](https://webassembly.org/) to Minecraft Java Edition datapacks.
Since WebAssembly is a well-supported target for many languages, this means that you can run code
written in e.g. C in Minecraft.

This was inspired by [Sethbling's Atari 2600 Datapack](https://youtu.be/mq7T5_xH24M) and is the (much, much improved) spiritual successor to [Langcraft](https://github.com/SuperTails/langcraft).

## Demonstrations

[Here](https://youtu.be/5jEyaGQFP0g) is a short demonstration of [a port of Minecraft 4k](https://github.com/SuperTails/Minecraft4k-For-Wasmcraft),
compiled using Wasmcraft, running in vanilla Minecraft. (That's right: Minecraft in Minecraft).

Alternatively, [here is a video](https://youtu.be/jrMrde9tQlg) of the CHIP-8 Emulator from [here](https://github.com/JamesGriffin/CHIP-8-Emulator) running Pong.

## Features

* Minimal porting required
  * Most non-hosted/freestanding code that doesn't use floats is compatible
  * Compatible with [newlib](https://sourceware.org/newlib/) to provide libc features like `printf` and `malloc`
* Common, well-supported, and stable source language
  * Can be targeted from C, C++, and Rust
* All WASM integer and control flow operations supported (and tested against the WebAssembly test suite)
* SSA-based optimizations
  * Strength reduction
  * Constant folding
  * Dead code elimination
  * Register allocation
* No game modification required
  * Compatible with vanilla Java Edition Minecraft 1.16+ (tested on 1.19+)

## Usage

Let's say we want to run the following program:

```c
/* foo.c */

#include "mcinterface.h"

// Since there is no standard library, we use `_start` instead of `main`
int _start() {
	for (int i = 0; i < 10; ++i) {
		print(i);
	}

	return 0;
}
```

First, compile it to an object file:

```bash
clang foo.c -target wasm32 -nostdlib -c -o foo.o
wasm-ld foo.o --lto-O3 --gc-sections --import-undefined -o foo.wasm
wasm2wat foo.wasm -o foo.wat # Optional, useful for debugging
```

Ensure you have Rust version >= 1.58 installed, which is very recent. To update Rust, run `rustup update`.
Then, simply navigate to the wasmcraft2 directory and run:

```bash
cargo run --release -- ../foo.wasm -O1 -o ../nameofdatapack`
```

This will create a datapack in the folder `nameofdatapack`, which can be directly placed in the datapacks folder
of any Minecraft Java Edition world (this has only been tested on 1.18/1.19, but should work on older and newer versions as well).

*Warning: all of the contents of the folder `../nameofdatapack` will be deleted.
Do **NOT** point it to a folder with important files!*

*Warning: due to limitations with Minecraft commands, this needs to fill a few chunks near 0, 0 with jukeboxes,
so do **NOT** run this in a world with builds you don't want destroyed!*

Run these commands:

`/gamerule maxCommandChainLength 100000000` (This only needs to be run once per world)

`/reload` (This only needs to be run when the datapack is changed while the world is open)

`/datapack enable "name/of/my/datapack"` (If it is not already enabled)

`/function wasmrunner:init`

`/function wasmrunner:_start`

And you should see the numbers 0 to 9 printed out in the chat.

To adjust the number of commands run per tick, you can set a value any time while the datapack is running:
```
scoreboard players set %%max_commands reg 30000
```
This number can be adjusted higher or lower depending on your system's performance.

## Inserting Sleep Calls

Currently, programs compiled under Wasmcraft have to have sleep calls manually inserted into them under certain circumstances.
This is because Minecraft tries to execute all commands in a datapack function call within a single game tick,
so in the worst case a very long-running function will freeze the game world indefinitely.

Wasmcraft currently inserts sleep checks before functions and inside of loops,
but long stretches of code that don't have any loops or function calls can end up having too many commands to work properly,
and some instructions are not command-counted properly (like memset).

In these cases, the `mc_sleep()` function provided in `mcinterface.h` will pause execution and resume it on the next game tick.
If sleep calls are inserted too frequently, the datapack will run very slowly.
However, if sleep calls are not inserted frequently enough, the game will lag or command execution will be canceled by the game entirely.

In order to determine if some code has sleep calls inserted frequently enough,
Wasmcraft can simulate the code using the following command:

```bash
cargo run --release -- ../foo.wasm -O1 -o ../nameofdatapack --no-persist-output --run-output
```

If `MaxTickCommandsRun` appears in the output,
too many commands were executed in a single tick and `mc_sleep()` must be inserted somewhere.
The provided stacktrace, combined with `print` statements or the `.wat` file, can be used to find the problematic code.
(Wasmcraft function IDs always match the ID of the corresponding function in the WebAssembly file).

When using the datapack in-game, press Ctrl-Alt-F3 to get a tick time graph (on the right side).
This can be used to find ticks that are running too slowly or are lagging the game.

Manual sleep calls are planned to be fixed in a future update.

## Using the C Standard Library

In order to use C library functions like `printf`, `malloc`, `open`, etc.,
projects can be compiled with the Newlib C stdlib implementation.

An example of how to do that can be found [here](https://github.com/SuperTails/wasmcraft-newlib-example)

## Related Tools

The [Wasmcraft Preview Simulator](https://github.com/SuperTails/wasmcraft-simulator) is useful for prototyping
and quickly testing programs before actually providing them to the Wasmcraft compiler.

## Limitations

* Floating point operations are not supported (yet).
Use fixed point operations instead, e.g. [libfixmath](https://github.com/PetteriAimonen/libfixmath)
* Bitwise operations and 64-bit divisions have to be emulated and are very slow.
* 8-bit and 16-bit accesses are fairly slow, and all memory accesses have not-insignificant overhead.
* Manual calls to `mc_sleep()` have to be inserted in some cases.
* Only a limited subset of Minecraft commands are available in the interface.

## License

Licensed under either of
* MIT License
* Apache License, Version 2.0