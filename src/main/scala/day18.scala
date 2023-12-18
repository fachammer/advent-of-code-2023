package day18

// part 1
def lagoonSizeByNormalInstructions(input: String) =
  lagoonSize:
    input.linesIterator.map:
      case s"$step $amount $_" => (step, amount.toLong)

// part 2
def lagoonSizeBySwappedInstructions(input: String) =
  val colorToStep = Map('0' -> "R", '1' -> "D", '2' -> "L", '3' -> "U")
  lagoonSize:
    input.linesIterator.map:
      case s"$_ $_ (#$color)" =>
        (colorToStep(color.last), java.lang.Long.parseLong(color.init, 16))

def lagoonSize(steps: Iterator[(String, Long)]) =
  val (_, _, doubleArea) = steps.foldLeft((0L, 0L, 2L)):
    case ((col, row, doubleArea), (step, amount)) =>
      val (nextCol, nextRow) = step match
        case "R" => (col + amount, row)
        case "D" => (col, row + amount)
        case "L" => (col - amount, row)
        case "U" => (col, row - amount)
      val determinant = col * nextRow - nextCol * row
      (nextCol, nextRow, doubleArea + determinant + amount)

  doubleArea / 2
