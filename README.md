# Source code of compiler project, in partial fulfilment of the course Compilation at ENS 2023-2024, L3, Winter semester

## Compilation and usage

Compilation requires build tool `dune` of version 5.10.3. The simulator is implemented in OCaml version 5.1.0. The implementation was compiled and tested in Linux Ubuntu 20.04.6 LTS through Window Subsystem for Linux. 
	
The source code is equipped with a `Makefile`, where command
- `make` builds the projects and results in an executable `ppurs.exe` available in the project's directory
- `make tests` builds the executable if necessary, and tests the compiler against the tests regarding syntactic analysis and typing analysis.
- `make clean` removes the build files and the executable.

The compiler has basic interface, of the form 
`./ppurs.exe [--parse-only] [--type-only] file.purs`, 
with
- `--parse-only`: flag to print only the netlist after scheduling;
- `--type-only`: flag to specify \texttt{nr}, the number of cycles to be simulated;
- `<file>.purs`, the `.purs` file containing the source code in `PetitPurescript`. For further information, consult the documentation provided in `./doc`, title `sujet-v3.pdf`. This produces an assembly, which can be compiled into an executable with `gcc -no-file <file>.s -o <file>.exe`

Testing status at 17.25, 21/1/2024 (CET).
- Part 1: 160/160, 100%
- Part 2: 91/92, 98%
- Part 3:
    - Compilation: 31/33, 93%
    - Production: 18/33, 54%
    - Correctness: 15/33, 45%