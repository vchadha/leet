object Solution {
    // Constants for the Sudoku board
    val BoardSize: Int = 9
    val BoxSize: Int = 3
    val SubBoxIndecies: Range = (0 until BoxSize * BoxSize)

    // Set of valid digits for Sudoku
    val PossibleDigits: Set[Char] = ('1' to '9').toSet

    // Character representing an empty cell
    val BlankCell: Char = '.'

    // TODO: type wrapper/alias for possible values for a cell
    // Empty as a type
    // NonEmpty as a type that contains the set of possible digits

    /**
      * Solves a given Sudoku puzzle by filling the empty cells.
      * Modifies the input board in-place.
      *
      * @param board A 9x9 2D array representing the Sudoku board, where empty cells are denoted by '.'.
      */
    def solveSudoku(board: Array[Array[Char]]): Unit = {
        
    }

    /**
      * Validates initial board state.
      * Checks:
      *     1. board size is 9 x 9
      *     2. board contains valid characters
      *     3. board has no duplicates in rows, cols, and subboxes
      *
      * @param board Board to check
      * @return True if board is valid else False
      */
    def validateBoard(board: Array[Array[Char]]): Boolean = {
        // Check if the board is 9x9
        (board.length != BoardSize || board.exists(_.length != BoardSize))
        // Check if all characters are valid (digits or blank)
        && board.forall(row => row.forall(cell => PossibleDigits.contains(cell) || cell == BlankCell))
        // Check for duplicates in rows, columns, and boxes
        && board.forall(row => isSetValid(row.toList))
        && board.transpose.forall(col => isSetValid(col.toList))
        && areSubBoxesValid(board)
    }

    /**
      * Determines if a set is valid by checking for no duplicates.
      * Filters out empty cells first.
      *
      * @param set List of values to check
      * @return True if set contains no duplicates after removing empty cells
      */
    def isSetValid(set: List[Char]): Boolean = {
        // Filter blank cells
        val nonBlankCells = set.filter(_ != BlankCell)

        // No duplicates in set
        nonBlankCells.distinct.length == nonBlankCells.length
    }

    // TODO: do i want lists or sets or indexed set?
    /**
      * Get lists of all values in each subbox.
      *
      * @param board Board to get values from
      * @return List of List of Chars. Each list contains the values of a subbox
      */
    def getSubBoxCells(board: Array[Array[Char]]): List[List[Char]] = {
        SubBoxIndecies.map(subBoxIndex =>
                val rowStart = (subBoxIndex / BoxSize) * BoxSize
                val colStart = (subBoxIndex % BoxSize) * BoxSize

                // Get all cells for box
                val subBoxCells = for {
                    row <- rowStart until rowStart + BoxSize
                    col <- colStart until colStart + BoxSize
                } yield board(row)(col)

                subBoxCells.toList
            ).toList
    }

    /**
      * Checks if subboxes do not contain duplicates and contain valid digits
      *
      * @param board Sudoku board
      * @return True if all subboxes are valid
      */
    def areSubBoxesValid(board: Array[Array[Char]]): Boolean = {
        getSubBoxCells(board).forall(subBoxCells => isSetValid(subBoxCells))
    }

    /**
      * Get the index of the sub square for a given row and column.
      *
      * | 0 | 1 | 2 |
      * |---|---|---|
      * | 3 | 4 | 5 |
      * |---|---|---|
      * | 6 | 7 | 8 |
      * 
      * @param row Row index
      * @param col Column index
      * @return Index of the sub square (0 to 8)
      */
    def getBoxIndex(row: Int, col: Int): Int =
        (row / BoxSize) * BoxSize + (col / BoxSize)

    /**
      * Get initial number sets for each row, column, subbox. Also get list of all blank locations.
      * Each number set contains all the non blanks for that row or column or subbox.
      *
      * @param board Board to get initial sets from
      * @return Tuple of row sets, column sets, box sets, and empty cell locations set
      */
    def initializeSolutionSets(board: Array[Array[Char]]): (
        List[Set[Char]],  // Row sets
        List[Set[Char]],  // Column sets
        List[Set[Char]],  // Box sets
        List[(Int, Int)]  // List of empty cell locations
    ) = {
        val rowSets = board.map(row => row.toSet - BlankCell).toList
        val colSets = board.transpose.map(col => col.toSet - BlankCell).toList
        val subBoxSets = getSubBoxCells(board).map(subBoxCells => subBoxCells.toSet - BlankCell).toList

        val emptyCellLocationSet =
            board.zipWithIndex
                .flatMap { case (row, rowIndex) => 
                        row.zipWithIndex.collect {
                            case (cell, colIndex) if cell == BlankCell => (rowIndex, colIndex)
                        }
                    }
            .toList

        (rowSets, colSets, subBoxSets, emptyCellLocationSet)
    }

}
