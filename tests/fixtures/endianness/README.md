This folder contains litmus tests for endianness analysis

- `equivalence.f90` is endian sensitive due to an EQUIVALENCE
- `common.f90` declares a COMMON block that is used by `common_receiver.f90` in a way that is endian sensitive.
- `files.f90` is endian sensitive through reading/writing data files.
- `bittest.f90` is endiand sensitive due to various bit-specific instrinsic operations.

Build with `gfortran X.f90 -o example` and run the resulting `example` to see the behaviour.
