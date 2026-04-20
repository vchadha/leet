# Scala Puzzles

Solutions to programming puzzles in Scala 3, focusing on type-safe,
functional design.

## Setup

**Requirements**: JDK 11+, sbt

1. Install [sdkman](https://sdkman.io/install/)
2. Install [scala and sbt](https://sdkman.io/usage/)

## Running

```bash
cd scala

sbt compile      # compile
sbt test         # run all tests
sbt ~test        # watch mode - reruns on file save
sbt scalafmtAll  # format all files
```

## Adding solutions
Create new package in scala/src directory for new puzzles.
