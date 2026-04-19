object Utils {
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
}