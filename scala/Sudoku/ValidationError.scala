sealed trait ValidationError {
  def message: String
}

object ValidationError {
  case object InvalidBoardSize extends ValidationError {
    def message = s"Board must be ${Constants.BoardSize}x${Constants.BoardSize}"
  }

  case class InvalidCell(row: Int, col: Int, char: Char)
      extends ValidationError {
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
