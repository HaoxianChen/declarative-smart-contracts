# Declarative smart contracts

A compiler that translates Declarative smart contracts
into Solidity programs.

# Install dependencies

Build tool: [SBT](https://www.scala-sbt.org/1.x/docs/Setup.html)

# Run

1. Generate Solidity programs: ``sbt "run test"``
2. Generate Solidity programs with instrumentations for run-time verification: ``sbt "run test-instrument"``

The generated programs are located at ``solidity/dsc`` and ``solidity/dsc-instrument``.

# Run in Docker
Alternatively, one could run it in docker.

First install Docker from [here](https://docs.docker.com/engine/install/).

1. Pull the docker image: ``docker pull hxchen/fse22-artifact``
2. Generate Solidity programs: `` docker run hxchen/fse22-artifact  sh -c "sbt 'run test'; cat solidity/dsc/*.sol"``
3. Generate Solidity programs with instrumentations for run-time verification: `` docker run hxchen/fse22-artifact  sh -c "sbt 'run test-instrument'; cat solidity/dsc-instrument/*.sol"``


# Example contracts

Five examples of declarative smart contrats are located in [benchmarks](benchmarks/).
