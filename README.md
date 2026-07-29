# Smart Contract Synthesis via Multi-modal Specifications

# Install dependencies

Build tool: [SBT](https://www.scala-sbt.org/1.x/docs/Setup.html)

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


## Running Synthesis Benchmarks

The [synthesis-benchmark](synthesis-benchmark) directory contains declarative smart contract examples and their corresponding benchmarks. Each subdirectory represents a specific contract and includes the following files:

- `schema.dl`: Defines the schema for the contract,
  including transaction records and contract states.
- `rules.dl`: Inference rules.
- `properties.dl`: Safety properties.


Run the following command to synthesize all benchmarks in the directory.
```shell
sbt run synthesis-all
```

## Output
The output is written to [synthesis-output](synthesis-output) directory.
Look for file with `*.sol` suffix.