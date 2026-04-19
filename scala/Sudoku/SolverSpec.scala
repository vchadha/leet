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
