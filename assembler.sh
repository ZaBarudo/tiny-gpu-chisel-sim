#!/bin/bash

# Usage: Tiny-GPU MachineCodeEmitter <asm_file> [--idx: Print instructions with index]

# Get the filename and options, ORDER MATTERS!
OPTIONS=${@:2}

# Run the MachineCodeEmitter using mill
./mill playground.test.runMain gpu.MachineCodeEmitter "$1" $OPTIONS