# Declarative smart contracts

A compiler that translates Declarative smart contracts
into Solidity programs.

# Install dependencies

Build tool: [SBT](https://www.scala-sbt.org/1.x/docs/Setup.html)

# Run

1. Generate Solidity programs: ``sbt "run test"``
2. Generate Solidity programs with instrumentations for run-time verification: ``sbt "run test-instrument"``

The generated programs are located at ``solidity/dsc`` and ``solidity/dsc-instrument``.

# Example contracts

Five examples of declarative smart contrats are located in [benchmarks](benchmarks/).

# Verification

## Setup Z3 

1. Download z3 [source](https://github.com/Z3Prover/z3).
2. Build z3 and generate Java binding: 
    ```
    cd z3
    python scripts/mk_make.py --java
    cd build
    make
    ```
3. Copy files from ``z3`` to ``dsc`` project directory:
    * copy ``com.microsoft.z3.jar`` to sub-directory called ``unmanaged``.
    * copy ``libz3.dylib`` and ``libz3java.dylib`` to the project directory.
4. Add the following line to ``build.sbt``:
    ```
    Compile / unmanagedJars += {
      baseDirectory.value / "unmanaged" / "com.microsoft.z3.jar"
    }
    ```
5. In sbt configuration, set working directory as the project directory, so that Java runtime can locate the two dylib file.
