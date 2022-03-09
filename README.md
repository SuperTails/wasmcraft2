# Wasmcraft

Have you ever wanted to run C/C++/Rust/WebAssembly applications in Minecraft? No? Well, now you can!

Wasmcraft is a compiler from [WebAssembly](https://webassembly.org/) to Minecraft Java Edition datapacks.
Since WebAssembly is a well-supported target for many languages, this means that you can run code
written in e.g. C in Minecraft.

## Demonstration

TODO:

## Features

* All integer operations supported
* (Relatively) efficient code generation
* Works most of the time

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

First, compile it to WebAssembly:

`clang foo.c -target wasm32 -nostdlib -o foo.wasm`

Ensure you have Rust version >= 1.58 installed, which is very recent. To update Rust, run `rustup update`.
Then, simply navigate to the wasmcraft2 directory and run:

`cargo run --release -- ../foo.wasm ../nameofdatapack`

This will create a datapack in the folder `nameofdatapack`, which can be directly placed in the datapacks folder
of any Minecraft Java Edition world (this has only been tested on 1.18, but should work on older versions as well).

*Note that due to limitations with Minecraft commands, this needs to fill a few chunks near 0, 0 with jukeboxes,
so do **NOT** run this in a world with builds you don't want destroyed!*

Run these commands:

`/gamerule maxCommandChainLength 1000000` (This only needs to be run once per world)

`/reload` (This only needs to be run when the datapack is changed while the world is open)

`/function wasmrunner:init`

`/function wasmrunner:_start`

And you should see the numbers 0 to 9 printed out in the chat.

## Limitations

* Floating point operations are not supported (yet).
Use fixed point operations instead, e.g. [libfixmath](https://github.com/PetteriAimonen/libfixmath)
* Bitwise operations and 64-bit divisions are *absurdly* slow.
* 8-bit and 16-bit accesses are fairly slow, and all memory accesses have not-insignificant overhead.
* Manual calls to `sleep()` have to be inserted in long-running code,
because Minecraft can only run a limited number of commands per tick.
* The API is very limited