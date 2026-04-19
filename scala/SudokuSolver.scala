import scala.collection.View.Fill
object Constants {
  // Constants for the Sudoku board
  val BoardSize: Int = 9
  val BoxSize: Int = 3
  val SubBoxIndices: Range = (0 until BoxSize * BoxSize)
  val ValidDigits: Set[Char] = ('1' to '9').toSet
  val BlankCellChar: Char = '.'
}

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
case object Invalid extends Cell
case class Filled(digit: Char) extends Cell

object Cell {
  def fromChar(c: Char): Cell = c match
    case Constants.BlankCellChar => Blank
    case d             => Filled.fromChar(d).getOrElse(Invalid)

  def toChar(cell: Cell): Either[ValidationError, Char] = cell match
    case Blank     => Right(Constants.BlankCellChar)
    case Filled(d) => Right(d)
    case Invalid => Left(ValidationError.InvalidCellConversion())
}

object Filled {
  val validFilled: Set[Filled] = Constants.ValidDigits.map(Filled.apply)

  def fromChar(c: Char): Option[Filled] =
    if Constants.ValidDigits.contains(c) then Some(Filled(c))
    else None
}

case class Location(row: Int, col: Int)

sealed trait ValidationError {
  def message: String
}

object ValidationError {
  case object InvalidBoardSize extends ValidationError {
    def message = s"Board must be ${Constants.BoardSize}x${Constants.BoardSize}"
  }

  case class InvalidCellConversion() extends ValidationError {
    def message = "Attempted to convert cell of type Invalid to Char"
  }

  case class InvalidCell(row: Int, col: Int, char: Char) extends ValidationError {
    def message = s"Invalid character '$char' at ($row, $col)"
  }

  case class DuplicateInRow(row: Int, value: Char) extends ValidationError {
    def message = s"Duplicate '$value' in row $row"
  }

  case class DuplicateInCol(col: Int, value: Char) extends ValidationError {
    def message = s"Duplicate '$value' in col $col"
  }

  case class DuplicateInBox(box: Int, value: Char) extends ValidationError {
    def message = s"Duplicate '$value' in box $box"
  }
}

object Solution {
  // TODO: make naming consistent

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
    validateBoard(cellBoard) match
      case Left(errors) =>
        val message = errors.map(_.message).mkString("\n")
        throw new IllegalArgumentException(s"Invalid board:\n$message")
      case Right(_) =>
        // Initialize sets for rows, columns, and subboxes, and get empty cell locations
        val (rowSets, colSets, boxSets, emptyCellLocationSet) =
          initializeSolutionSets(cellBoard)

        // Create a map of possible values for each empty cell
        val emptyCellSolutionSet = emptyCellLocationSet.map { location =>
          val boxIndex = getBoxIndex(location.row, location.col)

          val possibleValues =
            Filled.validFilled -- rowSets(location.row) -- colSets(location.col) -- boxSets(boxIndex)

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
          row <- 0 until Constants.BoardSize
          col <- 0 until Constants.BoardSize
        } board(row)(col) = Cell.toChar(cellBoard(row)(col))
  }

  private def findDuplicates(
    cells: List[Cell],
    makeError: (Char) => ValidationError
  ): List[ValidationError] = {
    val filled = cells.collect { case Filled(d) => d }
    filled
      .groupBy(identity)
      .collect { case (digit, occurrences) if occurrences.length > 1 => makeError(digit) }
      .toList
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
  def validateBoard(board: Array[Array[Cell]]): Either[List[ValidationError], Unit] = {
    // Check if the board is 9x9
    val dimensionErrors: List[ValidationError] =
      if board.length != Constants.BoardSize || board.exists(_.length != Constants.BoardSize)
      then List(ValidationError.InvalidBoardSize)
      else Nil

    val valueErrors: List[ValidationError] =
      (for {
        (row, rowIndex) <- board.zipWithIndex
        (cell, colIndex) <- row.zipWithIndex
        error <- cell match {
          case Blank     => None
          case Filled => None
          case Invalid => Some(ValidationError.InvalidCell(rowIndex, colIndex, '?'))
        }
      } yield error).toList

    val rowErrors: List[ValidationError] =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        findDuplicates(row.toList, char => ValidationError.DuplicateInRow(rowIndex, char))
      }.toList
    
    val colErrors: List[ValidationError] =
      board.transpose.zipWithIndex.flatMap { case (col, colIndex) =>
        findDuplicates(col.toList, char => ValidationError.DuplicateInCol(colIndex, char))
      }.toList

    val boxErrors: List[ValidationError] =
      getSubBoxCells(board).zipWithIndex.flatMap { case (box, boxIndex) =>
        findDuplicates(box, char => ValidationError.DuplicateInBox(boxIndex, char))
      }.toList

    val allErrors = dimensionErrors ++ valueErrors ++ rowErrors ++ colErrors ++ boxErrors

    if allErrors.isEmpty then Right(()) else Left(allErrors)
  }

  /** Get lists of all values in each subbox.
    *
    * @param board
    *   Board to get values from
    * @return
    *   List of List of Chars. Each list contains the values of a subbox
    */
  def getSubBoxCells(board: Array[Array[Cell]]): List[List[Cell]] = {
    Constants.SubBoxIndices
      .map(subBoxIndex =>
        val rowStart = (subBoxIndex / Constants.BoxSize) * Constants.BoxSize
        val colStart = (subBoxIndex % Constants.BoxSize) * Constants.BoxSize

        // Get all cells for box
        val subBoxCells = for {
          row <- rowStart until rowStart + Constants.BoxSize
          col <- colStart until colStart + Constants.BoxSize
        } yield board(row)(col)

        subBoxCells.toList
      )
      .toList
  }

  // TODO: do i need this?
  // TODO: think of types in the entire program - should i use something else?
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
  private def getBoxIndex(row: Int, col: Int): Int =
    (row / Constants.BoxSize) * Constants.BoxSize + (col / Constants.BoxSize)

  private def toFilledSet(cells: Iterable[Cell]): Set[Filled] =
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
  private def initializeSolutionSets(board: Array[Array[Cell]]): (
      Vector[Set[Filled]], // Row sets
      Vector[Set[Filled]], // Column sets
      Vector[Set[Filled]], // Box sets
      List[Location] // List of empty cell locations
  ) = {
    val rowSets = board.map(row => toFilledSet(row)).toVector
    val colSets = board.transpose.map(col => toFilledSet(col)).toVector
    val subBoxSets = getSubBoxCells(board).map(toFilledSet).toVector

    val emptyCellLocationSet =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cell, colIndex) if cell == Blank => Location(rowIndex, colIndex)
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
  private def populateBoard(
      board: Array[Array[Cell]],
      rowSets: Vector[Set[Filled]],
      colSets: Vector[Set[Filled]],
      boxSets: Vector[Set[Filled]],
      emptyCellLocationSet: List[Location],
      emptyCellSolutionSet: Map[
        Location,
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
            val row = bestBlank.row
            val col = bestBlank.col
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
                val sameRow = loc.row == row
                val sameCol = loc.col == col
                val sameBox = getBoxIndex(loc.row, loc.col) == boxIndex
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
