# Programming Puzzles

A collection of solutions to programming puzzles in Python and Scala.

## Structure

```
python/          Python solutions (flat files)
scala/           Scala solutions (sbt project, one package per puzzle)
```

## Solutions

### Python

| Puzzle | File | Complexity |
|--------|------|------------|
| 🎮 Conway's Game of Life | `python/game_of_life.py` | O(rows × cols) |
| 🧩 Sudoku Solver | `python/sudoku_solver.py` | Worst-case exponential |
| 🪞 Mirror Distance | `python/mirror_distance_int.py` | O(log n) time, O(1) space |
| 2️⃣ Two Sum | `python/two_sum.py` | O(n) time, O(n) space |
| ➕ Add Two Numbers | `python/add_two_numbers.py` | O(max(m,n)) |

### Scala

| Puzzle | Package | Complexity |
|--------|---------|------------|
| 🧩 Sudoku Solver | `sudoku` | Worst-case exponential, MRV heuristic |

## Requirements

### Python
- Python 3.13+
- No external dependencies

### Scala
- JDK 11+
- sbt — see [`scala/README.md`](scala/README.md) for setup
