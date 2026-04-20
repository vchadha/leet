package sudoku

class SolverSpec extends munit.FunSuite:

  // ── Helpers ────────────────────────────────────────────────────────────────

  def board(rows: String*): Array[Array[Char]] =
    rows.map(_.toCharArray).toArray

  def cellBoard(rows: String*): Array[Array[Cell]] =
    rows.map(_.map(Cell.fromChar).map(_.getOrElse(Blank)).toArray).toArray

  def assertBoardEquals(
      actual: Array[Array[Char]],
      expected: Array[Array[Char]]
  )(using munit.Location): Unit =
    assertEquals(
      actual.map(_.mkString).toSeq,
      expected.map(_.mkString).toSeq
    )

  // ── Solving ────────────────────────────────────────────────────────────────

  test("solves standard puzzle"):
    val b = board(
      "53..7....",
      "6..195...",
      ".98....6.",
      "8...6...3",
      "4..8.3..1",
      "7...2...6",
      ".6....28.",
      "...419..5",
      "....8..79"
    )
    Solution.solveSudoku(b)
    assertBoardEquals(
      b,
      board(
        "534678912",
        "672195348",
        "198342567",
        "859761423",
        "426853791",
        "713924856",
        "961537284",
        "287419635",
        "345286179"
      )
    )

  test("leaves already-solved board unchanged"):
    val b = board(
      "534678912",
      "672195348",
      "198342567",
      "859761423",
      "426853791",
      "713924856",
      "961537284",
      "287419635",
      "345286179"
    )
    val original = b.map(_.clone())
    Solution.solveSudoku(b)
    assertBoardEquals(b, original)

  test("solves board with one blank"):
    val b = board(
      "534678912",
      "672195348",
      "198342567",
      "859761423",
      "426853791",
      "713924856",
      "961537284",
      "287419635",
      "34528617." // last cell is '9'
    )
    Solution.solveSudoku(b)
    assertEquals(b(8)(8), '9')

  test("throws IllegalStateException when no solution exists"):
    // Structurally valid (no duplicates) but unsolvable
    val b = board(
      "53..7....",
      "6..195...",
      ".98....6.",
      "8...6...3",
      "4..8.3..1",
      "7...2...6",
      ".6....28.",
      "...419..5",
      "....8..7." // both 8 and 9 are forced into the same cell
    )
    // Replace the last two cells to force impossibility
    b(8)(7) = '8'
    b(8)(8) = '.'
    intercept[IllegalStateException]:
      Solution.solveSudoku(b)

  // ── ValidationError messages ───────────────────────────────────────────────

  test("ValidationError.InvalidBoardSize has correct message"):
    assertEquals(ValidationError.InvalidBoardSize.message, "Board must be 9x9")

  test("ValidationError.InvalidCell has correct message"):
    assertEquals(
      ValidationError.InvalidCell(3, 7, 'Z').message,
      "Invalid character 'Z' at (3, 7)"
    )

  test("ValidationError.DuplicateInRow has correct message"):
    assertEquals(ValidationError.DuplicateInRow(2, '4').message, "Duplicate '4' in row 2")

  test("ValidationError.DuplicateInCol has correct message"):
    assertEquals(ValidationError.DuplicateInCol(5, '9').message, "Duplicate '9' in col 5")

  test("ValidationError.DuplicateInBox has correct message"):
    assertEquals(ValidationError.DuplicateInBox(8, '1').message, "Duplicate '1' in box 8")

  // ── Cell ───────────────────────────────────────────────────────────────────

  test("Cell.fromChar converts all valid digits"):
    ('1' to '9').foreach { d =>
      assertEquals(Cell.fromChar(d), Some(Filled(d)), s"Expected Some(Filled('$d'))")
    }

  test("Cell.fromChar returns Some(Blank) for '.'"):
    assertEquals(Cell.fromChar('.'), Some(Blank))

  test("Cell.fromChar returns None for invalid characters"):
    List('0', 'A', ' ', '!').foreach { c =>
      assertEquals(Cell.fromChar(c), None, s"Expected None for '$c'")
    }

  // ── Candidates ─────────────────────────────────────────────────────────────

  test("Candidates.apply returns Empty for empty set"):
    assertEquals(Candidates(Set.empty[Filled]), Empty)

  test("Candidates.apply returns NonEmpty for non-empty set"):
    assertEquals(
      Candidates(Set(Filled('1'), Filled('2'))),
      NonEmpty(Set(Filled('1'), Filled('2')))
    )

  test("Candidates.remove collapses to Empty when last value removed"):
    assertEquals(
      Candidates.remove(NonEmpty(Set(Filled('5'))), Filled('5')),
      Empty
    )

  test("Candidates.remove from Empty stays Empty"):
    assertEquals(Candidates.remove(Empty, Filled('3')), Empty)

  test("Candidates.size returns 0 for Empty"):
    assertEquals(Candidates.size(Empty), 0)

  test("Candidates.size returns element count for NonEmpty"):
    assertEquals(Candidates.size(NonEmpty(Set(Filled('1'), Filled('3'), Filled('7')))), 3)

  // ── Location ───────────────────────────────────────────────────────────────

  test("Location.isPeerOf: same row is peer"):
    assert(Location(0, 0).isPeerOf(Location(0, 8)))

  test("Location.isPeerOf: same col is peer"):
    assert(Location(0, 0).isPeerOf(Location(8, 0)))

  test("Location.isPeerOf: same box is peer"):
    assert(Location(0, 0).isPeerOf(Location(2, 2)))

  test("Location.isPeerOf: different row, col, and box is not peer"):
    assert(!Location(0, 0).isPeerOf(Location(4, 4)))

  test("Location.isPeerOf: is symmetric"):
    val a = Location(0, 0)
    val b = Location(0, 5)
    assertEquals(a.isPeerOf(b), b.isPeerOf(a))

  test("Location.boxIndex: corners map to correct boxes"):
    assertEquals(Location(0, 0).subBoxIndex, 0)
    assertEquals(Location(0, 8).subBoxIndex, 2)
    assertEquals(Location(8, 0).subBoxIndex, 6)
    assertEquals(Location(8, 8).subBoxIndex, 8)
    assertEquals(Location(4, 4).subBoxIndex, 4)
