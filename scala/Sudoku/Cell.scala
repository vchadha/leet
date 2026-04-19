// Sealed trait to represent cell values
sealed trait Cell
case object Blank extends Cell
case class Filled(digit: Char) extends Cell

object Cell {
  def fromChar(c: Char): Option[Cell] = c match
    case Constants.BlankCellChar => Some(Blank)
    case d                       => Filled.fromChar(d)

  def toChar(cell: Cell): Char = cell match
    case Blank     => Constants.BlankCellChar
    case Filled(d) => d
}

object Filled {
  val validFilled: Set[Filled] = Constants.ValidDigits.map(Filled.apply)

  def fromChar(c: Char): Option[Filled] =
    if Constants.ValidDigits.contains(c) then Some(Filled(c))
    else None
}

object CellHelpers {
  def toFilledSet(cells: Iterable[Cell]): Set[Filled] =
    cells.collect { case f: Filled => f }.toSet
}
