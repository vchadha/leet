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
  def getBoxIndex(loc: Location): Int =
    (loc.row / Constants.BoxSize) * Constants.BoxSize + (loc.col / Constants.BoxSize)
}