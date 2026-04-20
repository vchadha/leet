package sudoku

// Output of solver
// Map of blank locations to filled number
type SolutionMap = Map[Location, Filled]

object Solution {

  /** Solves a given Sudoku puzzle by filling the blank cells. Modifies the input board on return.
    *
    * @param board
    *   A 9x9 2D array representing the Sudoku board, where blank cells are denoted by '.'.
    */
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    val cellBoard = (
      for {
        cellBoard <- Validation.convertBoard(board)
        _         <- Validation.validateBoard(cellBoard)
      } yield cellBoard
    ) match
      case Left(errors) =>
        val message = errors.map(_.message).mkString("\n")
        throw new IllegalArgumentException(s"Invalid board:\n$message")
      case Right(board) => board

    // Initialize solution set
    val initialSolverState = SolverState.from(cellBoard)

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

  private def solve(solverState: SolverState): Option[SolutionMap] =
    // If there are no blank cells to traverse return empty map
    // There are no more cells to place
    if solverState.blankCells.isEmpty then
      Some(Map.empty) // TODO: replace this with Solution.Empty or something?

    // Otherwise we must place a value in the next blank
    else
      // Find blank cell with the least number of candidates to try first
      val bestBlank =
        solverState.blankCells.minBy(loc => Candidates.size(solverState.candidates(loc)))

      solverState.candidates(bestBlank) match
        // No candidates for this cell, backtrack
        case Empty => None

        // Try each possible value for this cell
        case NonEmpty(values) => 
          values.collectFirst(
            Function.unlift { possibleValue =>
              // Remove best blank from list of location
              val newBlankLocations =
                solverState.blankCells.filter(_ != bestBlank)

              // Remove best blank from list of candidates
              // and update each peer
              val newCandidates =
                (solverState.candidates - bestBlank).map { case (loc, candidates) =>
                  if loc.isPeerOf(bestBlank) then
                    loc -> Candidates.remove(
                      candidates = candidates,
                      value = possibleValue
                    )
                  // If not a peer, do not update
                  else loc -> candidates
                }

              solve(SolverState(newBlankLocations, newCandidates)).map(_ + (bestBlank -> possibleValue))
            }
          )
}
