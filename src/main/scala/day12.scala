package day12

// part 1
def sumOfArrangements(input: String) =
  input.linesIterator.map(parseLine).map(arrangements).sum

lazy val arrangements: ((String, Seq[Int])) => Long = memoize: (line, sizes) =>
  if sizes.isEmpty then if line.contains('#') then 0L else 1L
  else if line.length < sizes.head then 0L
  else
    line.head match
      case '.' => arrangements((line.tail, sizes))
      case '#' =>
        val (sizeBlock, rest) = line.splitAt(sizes.head)
        if sizeBlock.contains('.') || rest.nonEmpty && rest.head == '#' then 0L
        else arrangements(('.' +: rest.tail, sizes.tail))
      case '?' =>
        arrangements(('.' +: line.tail, sizes))
          + arrangements(('#' +: line.tail, sizes))

def memoize[T, R](f: T => R): T => R =
  val memo = scala.collection.mutable.Map[T, R]()
  x => memo.getOrElseUpdate(x, f(x))

def parseLine(line: String) =
  val s"$springs $constraints" = line: @unchecked
  val damageGroupSizes         = constraints.split(',').map(_.toInt)
  (springs, damageGroupSizes.toSeq)

// part 2
def sumOfUnfoldedSpringConfigurations(input: String) =
  sumOfArrangements(input.linesIterator.map(unfold).mkString("\n"))

def unfold(line: String): String =
  val s"$springs $constraints" = line: @unchecked
  val unfoldedSprings          = Seq.fill(5)(springs).mkString("?")
  val unfoldedConstraints      = Seq.fill(5)(constraints).mkString(",")
  s"$unfoldedSprings $unfoldedConstraints"
