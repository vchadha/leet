package sudoku

// Sealed trait to represent candidates for a cell
sealed trait Candidates
case object Empty                        extends Candidates
case class NonEmpty(values: Set[Filled]) extends Candidates

object Candidates {

  // Smart constructor — returns Empty if the set is blank
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
