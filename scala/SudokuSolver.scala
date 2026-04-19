// Sealed trait to represent candidates for a cell
sealed trait Candidates
case object Empty extends Candidates
case class NonEmpty(values: Set[Filled]) extends Candidates

object Candidates {
  // Smart constructor — returns Empty if the set is empty
  def apply(values: Set[Filled]): Candidates =
    if values.isEmpty then Empty
    else NonEmpty(values)

  // Remove a value, collapsing to Empty if needed
  def remove(candidates: Candidates, value: Filled): Candidates =
    candidates match
      case Empty            => Empty
      case NonEmpty(values) => Candidates(values - value)

  def size(candidates: Candidates): Int =
    candidates match
      case Empty            => 0
      case NonEmpty(values) => values.size
}

// Sealed trait to represent cell values
sealed trait Cell
case object Blank extends Cell
case class Filled(digit: Char) extends Cell

object Cell {
  val BlankCellChar: Char = '.'

  def fromChar(c: Char): Cell = c match
    case BlankCellChar => Blank
    case d             => Filled(d)

  def toChar(cell: Cell): Char = cell match
    case Blank     => BlankCellChar
    case Filled(d) => d
}

object Filled {
  val validDigits: Set[Char] = ('1' to '9').toSet

  def fromChar(c: Char): Option[Filled] =
    if validDigits.contains(c) then Some(Filled(c))
    else None
}

object Solution {
  // Constants for the Sudoku board
  val BoardSize: Int = 9
  val BoxSize: Int = 3
  val SubBoxIndecies: Range = (0 until BoxSize * BoxSize)

  // Set of valid digits for Sudoku
  // TODO: should this be a set? or something else? indexedSeq?
  val PossibleDigits: Set[Filled] = ('1' to '9').map(Filled.apply).toSet

  // Character representing an empty cell
  // TODO: do i still need this?
  // val BlankCell: Cell = Blank

  // TODO: make naming consistent
  // TODO: improve error handling to give more information - use either? zio style? validation? zio validation?

  /** Solves a given Sudoku puzzle by filling the empty cells. Modifies the
    * input board in-place.
    *
    * @param board
    *   A 9x9 2D array representing the Sudoku board, where empty cells are
    *   denoted by '.'.
    */
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    // Convert board characters to Cell types for easier processing
    val cellBoard: Array[Array[Cell]] = board.map(_.map(Cell.fromChar))

    // Validate the initial board state
    if !validateBoard(cellBoard) then
      throw new IllegalArgumentException("Invalid Sudoku board")

    // Initialize sets for rows, columns, and subboxes, and get empty cell locations
    val (rowSets, colSets, boxSets, emptyCellLocationSet) =
      initializeSolutionSets(cellBoard)

    // Create a map of possible values for each empty cell
    val emptyCellSolutionSet = emptyCellLocationSet.map { location =>
      val row = location._1
      val col = location._2
      val boxIndex = getBoxIndex(row, col)

      val possibleValues =
        PossibleDigits -- rowSets(row) -- colSets(col) -- boxSets(boxIndex)

      location -> Candidates(possibleValues)
    }.toMap

    // Populate the board with values until we find a solution
    if !populateBoard(
        cellBoard,
        rowSets,
        colSets,
        boxSets,
        emptyCellLocationSet,
        emptyCellSolutionSet
      )
    then
      throw new IllegalStateException(
        "No solution exists for the given Sudoku board"
      )

    // Convert the cell board back to characters for the final output
    for {
      row <- 0 until BoardSize
      col <- 0 until BoardSize
    } board(row)(col) = Cell.toChar(cellBoard(row)(col))
  }

  /** Validates initial board state. Checks:
    *   1. board size is 9 x 9
    *   2. board contains valid characters
    *   3. board has no duplicates in rows, cols, and subboxes
    *
    * @param board
    *   Board to check
    * @return
    *   True if board is valid else False
    */
  def validateBoard(board: Array[Array[Cell]]): Boolean = {
    // Check if the board is 9x9
    (board.length == BoardSize && board.forall(_.length == BoardSize))
    // Check if all characters are valid (digits or blank)
    && board.forall(row =>
      row.forall {
        case Blank     => true // blank is always valid
        case Filled(d) =>
          Filled.validDigits.contains(d) // check the char inside
      }
    )
    // Check for duplicates in rows, columns, and boxes
    && board.forall(row => isSetValid(row.toList))
    && board.transpose.forall(col => isSetValid(col.toList))
    && areSubBoxesValid(board)
  }

  /** Determines if a set is valid by checking for no duplicates. Filters out
    * empty cells first.
    *
    * @param set
    *   List of values to check
    * @return
    *   True if set contains no duplicates after removing empty cells
    */
  def isSetValid(set: List[Cell]): Boolean = {
    // Filter blank cells
    val nonBlankCells = set.filter(_ != Blank)

    // No duplicates in set
    nonBlankCells.distinct.length == nonBlankCells.length
  }

  // TODO: do i want lists or sets or indexed set?
  /** Get lists of all values in each subbox.
    *
    * @param board
    *   Board to get values from
    * @return
    *   List of List of Chars. Each list contains the values of a subbox
    */
  def getSubBoxCells(board: Array[Array[Cell]]): List[List[Cell]] = {
    SubBoxIndecies
      .map(subBoxIndex =>
        val rowStart = (subBoxIndex / BoxSize) * BoxSize
        val colStart = (subBoxIndex % BoxSize) * BoxSize

        // Get all cells for box
        val subBoxCells = for {
          row <- rowStart until rowStart + BoxSize
          col <- colStart until colStart + BoxSize
        } yield board(row)(col)

        subBoxCells.toList
      )
      .toList
  }

  /** Checks if subboxes do not contain duplicates and contain valid digits
    *
    * @param board
    *   Sudoku board
    * @return
    *   True if all subboxes are valid
    */
  def areSubBoxesValid(board: Array[Array[Cell]]): Boolean = {
    getSubBoxCells(board).forall(subBoxCells => isSetValid(subBoxCells))
  }

  // TODO: do i need this?
  /** Get the index of the sub square for a given row and column.
    *
    * | 0   | 1   | 2   |
    * |:----|:----|:----|
    * | 3   | 4   | 5   |
    * | --- | --- | --- |
    * | 6   | 7   | 8   |
    *
    * @param row
    *   Row index
    * @param col
    *   Column index
    * @return
    *   Index of the sub square (0 to 8)
    */
  def getBoxIndex(row: Int, col: Int): Int =
    (row / BoxSize) * BoxSize + (col / BoxSize)

  def toFilledSet(cells: Iterable[Cell]): Set[Filled] =
    cells.collect { case f: Filled => f }.toSet

  /** Get initial number sets for each row, column, subbox. Also get list of all
    * blank locations. Each number set contains all the non blanks for that row
    * or column or subbox.
    *
    * @param board
    *   Board to get initial sets from
    * @return
    *   Tuple of row sets, column sets, box sets, and empty cell locations set
    */
  def initializeSolutionSets(board: Array[Array[Cell]]): (
      Vector[Set[Filled]], // Row sets
      Vector[Set[Filled]], // Column sets
      Vector[Set[Filled]], // Box sets
      List[(Int, Int)] // List of empty cell locations
  ) = {
    val rowSets = board.map(row => toFilledSet(row)).toVector
    val colSets = board.transpose.map(col => toFilledSet(col)).toVector
    val subBoxSets = getSubBoxCells(board).map(toFilledSet).toVector

    val emptyCellLocationSet =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cell, colIndex) if cell == Blank => (rowIndex, colIndex)
        }
      }.toList

    (rowSets, colSets, subBoxSets, emptyCellLocationSet)
  }

  /** Populate board with values until we find solution
    *
    * @param board
    *   Board to solve
    * @param rowSets
    *   Set of all numbers in each row
    * @param colSets
    *   Set of all numbers in each column
    * @param boxSets
    *   Set of all numbers in each subbox
    * @param emptyCellLocationSet
    *   Set of all empty locations
    * @param emptyCellSolutionSet
    *   Set of all possible values for each empty cell
    * @return
    *   True if board is solved, else False
    */
  def populateBoard(
      board: Array[Array[Cell]],
      rowSets: Vector[Set[Filled]],
      colSets: Vector[Set[Filled]],
      boxSets: Vector[Set[Filled]],
      emptyCellLocationSet: List[(Int, Int)],
      emptyCellSolutionSet: Map[
        (Int, Int),
        Candidates
      ] // TODO: make wrapper around location int, int?
  ): Boolean = {
    // Base case - if there are no more blank spaces, we have solved the board
    if emptyCellLocationSet.isEmpty then true
    // Otherwise we must place a value in the next blank
    else
      // Find blank cell with the least number of candidates to try first
      val bestBlank = emptyCellLocationSet.minBy(loc =>
        Candidates.size(emptyCellSolutionSet(loc))
      )

      emptyCellSolutionSet(bestBlank) match
        case Empty            => false // No candidates for this cell, backtrack
        case NonEmpty(values) =>
          // Try each possible value for this cell
          values.exists { possibleValue =>
            val row = bestBlank._1
            val col = bestBlank._2
            val boxIndex = getBoxIndex(row, col)

            // Place the value
            board(row)(col) = possibleValue

            // Update solution sets
            val newRowSets = rowSets.updated(row, rowSets(row) + possibleValue)
            val newColSets = colSets.updated(col, colSets(col) + possibleValue)
            val newBoxSets =
              boxSets.updated(boxIndex, boxSets(boxIndex) + possibleValue)
            val newEmptyCellLocationSet =
              emptyCellLocationSet.filter(_ != bestBlank)
            val newEmptyCellSolutionSet =
              (emptyCellSolutionSet - bestBlank).map { case (loc, candidates) =>
                val sameRow = loc._1 == row
                val sameCol = loc._2 == col
                val sameBox = getBoxIndex(loc._1, loc._2) == boxIndex
                val isPeer = sameRow || sameCol || sameBox
                if isPeer then
                  loc -> Candidates.remove(
                    candidates = candidates,
                    value = possibleValue
                  )
                else loc -> candidates
              }

            // Recursively try to solve the board with this value
            val isSolved = populateBoard(
              board,
              newRowSets,
              newColSets,
              newBoxSets,
              newEmptyCellLocationSet,
              newEmptyCellSolutionSet
            )

            if !isSolved then
              // Backtrack if this path didn't work
              board(row)(col) = Blank

            isSolved
          }
  }

}

// TODO: make a better framework for testing scala code
object SudokuTest {
  def main(args: Array[String]): Unit = {
    val board: Array[Array[Char]] = Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    )

    val expected: Array[Array[Char]] = Array(
      Array('5', '3', '4', '6', '7', '8', '9', '1', '2'),
      Array('6', '7', '2', '1', '9', '5', '3', '4', '8'),
      Array('1', '9', '8', '3', '4', '2', '5', '6', '7'),
      Array('8', '5', '9', '7', '6', '1', '4', '2', '3'),
      Array('4', '2', '6', '8', '5', '3', '7', '9', '1'),
      Array('7', '1', '3', '9', '2', '4', '8', '5', '6'),
      Array('9', '6', '1', '5', '3', '7', '2', '8', '4'),
      Array('2', '8', '7', '4', '1', '9', '6', '3', '5'),
      Array('3', '4', '5', '2', '8', '6', '1', '7', '9')
    )

    println("Input board:")
    printBoard(board)

    Solution.solveSudoku(board)

    println("\nSolved board:")
    printBoard(board)

    val passed = board.zip(expected).forall { case (solvedRow, expectedRow) =>
      solvedRow.sameElements(expectedRow)
    }

    println(s"\nTest ${if passed then "PASSED ✓" else "FAILED ✗"}")

    if !passed then
      println("\nExpected board:")
      printBoard(expected)
  }

  def printBoard(board: Array[Array[Char]]): Unit = {
    board.zipWithIndex.foreach { case (row, rowIdx) =>
      if rowIdx % 3 == 0 && rowIdx != 0 then println("------+-------+------")
      val rowStr = row.zipWithIndex
        .map { case (cell, colIdx) =>
          if colIdx % 3 == 0 && colIdx != 0 then s"| $cell" else s"$cell"
        }
        .mkString(" ")
      println(rowStr)
    }
  }
}
