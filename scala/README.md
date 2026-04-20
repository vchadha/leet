# Scala Puzzles

Solutions to programming puzzles in Scala 3, focusing on type-safe,
functional design.

## Setup

**Requirements**: JDK 11+, [sbt](https://www.scala-sbt.org/download)

```bash
# macOS
brew install sbt

# or download directly
https://www.scala-sbt.org/download
```

## Running

```bash
cd scala

sbt compile      # compile
sbt test         # run all tests
sbt ~test        # watch mode — reruns on file save
sbt scalafmtAll  # format all files
```

## Solutions

### 🧩 Sudoku Solver

**Package**: `sudoku`  
**Problem**: Solve a 9×9 Sudoku puzzle.  
**Approach**: Backtracking with constraint propagation and minimum
remaining value (MRV) heuristic.  
**Complexity**: Worst-case exponential, but MRV dramatically reduces
search space in practice.

#### Design notes

- Validates input before solving — reports all errors with positions
- Converts `Char` board to typed ADTs at the boundary; raw types never
  enter the solver
- Solver carries only `SolverState(blankCells, candidates)` — no mutable
  board threaded through recursion
- Backtracking is free — each recursive branch receives an immutable
  `SolverState`, no undo step needed
- Row/col/box constraint sets are computed once to seed initial candidates,
  then discarded — all constraint information lives in the candidates map

```
validateAndConvertBoard  →  buildSolverState  →  solve  →  write-back
Array[Array[Char]]           SolverState          Solution   Array[Array[Char]]
```
