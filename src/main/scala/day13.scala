package day13

// part 1
enum Reflection:
  case Horizontal(val row: Int)
  case Vertical(val col: Int)
type Pattern = Vector[Vector[Char]]
def summarize(input: String) = patterns(input).map(reflectionCount).sum

def patterns(input: String) =
  input.split("\n\n").map(_.linesIterator.map(_.toVector).toVector)

def reflectionCount(pattern: Pattern): Int =
  reflections(pattern).headOption.map(reflectionValue).getOrElse(0)

def reflectionValue(r: Reflection): Int =
  r match
    case Reflection.Horizontal(row) => row * 100
    case Reflection.Vertical(col)   => col

def reflections(pattern: Pattern): Seq[Reflection] =
  horizontalReflectionRows(pattern).map(Reflection.Horizontal(_)) ++
    verticalReflectionColumns(pattern).map(Reflection.Vertical(_))

def verticalReflectionColumns(pattern: Pattern): Seq[Int] =
  horizontalReflectionRows(pattern.transpose)

def horizontalReflectionRows(pattern: Pattern): Seq[Int] =
  def forwardReflectionRow(p: Pattern) = (1 to p.length / 2).find { i =>
    p.take(i).sameElements(p.drop(i).take(i).reverse)
  }

  val result  = forwardReflectionRow(pattern)
  val reverse = forwardReflectionRow(pattern.reverse).map(pattern.length - _)
  Seq(result, reverse).flatten

// part 2
def summarizeWithSmudgeRemoved(input: String) =
  patterns(input).map(reflectionWithSmudgeRemoved).map(reflectionValue).sum

def reflectionWithSmudgeRemoved(pattern: Pattern): Reflection =
  val reflection = reflections(pattern).head
  val differentReflections =
    for
      row <- 0 until pattern.length
      col <- 0 until pattern(0).length
      r   <- reflections(withoutSmudgeAt(pattern, row, col)) if r != reflection
    yield r
  differentReflections.head

def withoutSmudgeAt(pattern: Pattern, row: Int, col: Int): Pattern =
  val opposite = if pattern(row)(col) == '.' then '#' else '.'
  pattern.updated(row, pattern(row).updated(col, opposite))
