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
