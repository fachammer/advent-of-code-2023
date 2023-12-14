package day12

// part 1
def sumOfSpringConfigurations(input: String) =
  input.linesIterator.map(numberOfSpringConfigurations).sum

def numberOfSpringConfigurations(line: String) =
  val (springs, damageGroupSizes) = parseLine(line)

  val memo = scala.collection.mutable.Map[(String, Seq[Int]), Long]()
  def arrangements(line: String, sizes: Seq[Int]): Long =
    val memoValue = memo.get((line, sizes))
    if memoValue.isDefined then return memoValue.get

    val result =
      if sizes.isEmpty then if line.contains('#') then 0L else 1L
      else if line.length < sizes.head then 0L
      else
        line.head match
          case '.' => arrangements(line.tail, sizes)
          case '#' =>
            val (sizeBlock, rest) = line.splitAt(sizes.head)
            if sizeBlock.contains('.')
              || rest.headOption.map(_ == '#').getOrElse(false)
            then 0L
            else arrangements('.' +: rest.tail, sizes.tail)
          case '?' =>
            arrangements('.' +: line.tail, sizes) +
              arrangements('#' +: line.tail, sizes)

    memo((line, sizes)) = result
    result

  arrangements(springs, damageGroupSizes)

def parseLine(line: String) =
  val s"$springs $constraints" = line: @unchecked
  val damageGroupSizes         = constraints.split(',').map(_.toInt)
  (springs, damageGroupSizes.toSeq)

// part 2
def sumOfUnfoldedSpringConfigurations(input: String) =
  input.linesIterator.map(unfold).map(numberOfSpringConfigurations).sum

def unfold(line: String): String =
  val s"$springs $constraints" = line: @unchecked
  val unfoldedSprings          = repeat(springs, 5).mkString("?")
  val unfoldedConstraints      = repeat(constraints, 5).mkString(",")
  s"$unfoldedSprings $unfoldedConstraints"

def repeat[T](x: T, n: Int) = Iterator.continually(x).take(n)
