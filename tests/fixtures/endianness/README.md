This folder contains litmus tests for endianness analysis

- [equivalence.f90] is endian sensitive due to an EQUIVALENCE
- [common.f90] declares a COMMON block that is used by [common_receive.f90] in a way that is endian sensitive.
- [files.f90] is endian sensitive through reading/writing data files.

Build with `gfortran X.f90` to get `a.out` to see the behaviour.