# Programming Puzzles

A collection of solutions to programming puzzles in Python and Scala.

## Structure

```
python/          Python solutions (flat files)
scala/           Scala solutions (sbt project, one package per puzzle)
```

## Solutions

### Python

| Puzzle | Python File | Scala File | Complexity |
|--------|------|------------|-----|
| 🎮 Conway's Game of Life | `game_of_life.py` | | O(rows × cols) |
| 🧩 Sudoku Solver | `sudoku_solver.py` | `sudoku/Solver.scala` | Worst-case exponential |
| 🪞 Mirror Distance | `mirror_distance_int.py` | | O(log n) time, O(1) space |
| 2️⃣ Two Sum | `two_sum.py` | | O(n) time, O(n) space |
| ➕ Add Two Numbers | `add_two_numbers.py` | | O(max(m,n)) |

## Requirements

### Python
- Python 3.13+
- No external dependencies

### Scala
- JDK 11+
- sbt/scala — see [`scala/README.md`](scala/README.md) for setup
