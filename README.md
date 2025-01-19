A Chisel implementation of the `tiny-gpu` project. (https://github.com/adam-maj/tiny-gpu.git)
=======================

- [x] Implement the RTL in Chisel
- [ ] Implement the testbench using Verilator
- [ ] Implement the UVM like testbench using ChiselVerify
- [x] Implement the tiny GPU's virtual machine, including a assembler

## Getting Started

```bash
make test
```

To generate Verilog:
```bash
make verilog
```

## A naive approach to test this GPU
Implement software simulators (Model) for each hardware modules.
Generate random inputs for both hardware modules and software models, and see if the outputs can be matched.
Then combine software models like connecting each hardware modules to build models for those complex modules (e.g. Core and Gpu).

