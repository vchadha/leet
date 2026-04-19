/** Case class to be propogated through recursive calls.
  *
  * @param blankCells
  *   List of locations of blank cells
  * @param candidates
  *   Map of location to possible candidates for that location
  */
case class SolverState(
    blankCells: List[Location],
    candidates: Map[Location, Candidates]
)

// Output of solver
// Map of blank locations to filled number
type Solution = Map[Location, Filled]

// TODO: move to file with helper function to check if two locs are peers
case class Location(row: Int, col: Int)

object Solution {

  /** Solves a given Sudoku puzzle by filling the blank cells. Modifies the
    * input board on return.
    *
    * @param board
    *   A 9x9 2D array representing the Sudoku board, where blank cells are
    *   denoted by '.'.
    */
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    val cellBoard = Validation.validateAndConvertBoard(board) match
      case Left(errors) =>
        val message = errors.map(_.message).mkString("\n")
        throw new IllegalArgumentException(s"Invalid board:\n$message")
      case Right(board) => board

    // Initialize solution set
    val initialSolverState = buildSolverState(cellBoard)

    // Solve board
    val solution = solve(initialSolverState)

    // Fail if no solution found or update input board with final answers
    solution match
      case None =>
        throw new IllegalStateException(
          "No solution exists for the given Sudoku board"
        )
      case Some(soln) =>
        soln.foreach { case (loc, Filled(d)) => board(loc.row)(loc.col) = d }
  }

  private def buildSolverState(board: Array[Array[Cell]]): SolverState = {
    val rowSets = board.map(row => CellHelpers.toFilledSet(row)).toVector
    val colSets =
      board.transpose.map(col => CellHelpers.toFilledSet(col)).toVector
    val subBoxSets =
      Utils.getSubBoxCells(board).map(CellHelpers.toFilledSet).toVector

    val blankCells =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cell, colIndex) if cell == Blank => Location(rowIndex, colIndex)
        }
      }.toList

    // Create a map of possible values for each blank cell
    val candidates = blankCells.map { location =>
      val boxIndex = Utils.getBoxIndex(location)

      val possibleValues =
        Filled.validFilled
          -- rowSets(location.row)
          -- colSets(location.col)
          -- subBoxSets(boxIndex)

      location -> Candidates(possibleValues)
    }.toMap

    SolverState(
      blankCells = blankCells,
      candidates = candidates
    )
  }

  private def solve(solverState: SolverState): Option[Solution] = {
    // If there are no blank cells to traverse return empty map
    // There are no more cells to place
    if solverState.blankCells.isEmpty then
      Some(Map.empty) // TODO: replace this with Solution.Empty or something?

    // Otherwise we must place a value in the next blank
    else
      // Find blank cell with the least number of candidates to try first
      val bestBlank = solverState.blankCells.minBy(loc =>
        Candidates.size(solverState.candidates(loc))
      )

      solverState.candidates(bestBlank) match
        // No candidates for this cell, backtrack
        case Empty => None

        // Try each possible value for this cell
        case NonEmpty(values) =>
          values.foldLeft(Option.empty[Solution]) {
            case (found @ Some(_), _)  => found
            case (None, possibleValue) =>
              val row = bestBlank.row
              val col = bestBlank.col
              val boxIndex = Utils.getBoxIndex(bestBlank)

              // Update solution sets
              val newBlankLocations =
                solverState.blankCells.filter(_ != bestBlank)
              val newCandidates =
                (solverState.candidates - bestBlank).map {
                  case (loc, candidates) =>
                    val sameRow = loc.row == row
                    val sameCol = loc.col == col
                    val sameBox = Utils.getBoxIndex(loc) == boxIndex
                    val isPeer = sameRow || sameCol || sameBox
                    if isPeer then
                      loc -> Candidates.remove(
                        candidates = candidates,
                        value = possibleValue
                      )
                    else loc -> candidates
                }

              solve(SolverState(newBlankLocations, newCandidates)).map(
                _ + (bestBlank -> possibleValue)
              )
          }
  }

}
