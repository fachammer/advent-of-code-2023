package day12

import scala.util.Try
import debug.*

// part 1
def sumOfSpringConfigurations(input: String) =
  input.linesIterator.map(numberOfSpringConfigurations).sum

def processDamagesFromFront(damageGroupSizes: Array[Int], s: String) =
  import scala.collection.mutable
  val processedString = mutable.StringBuilder(s)
  def canProcess =
    val solvableIndex = processedString.indexOf("#?")
    solvableIndex != -1 && solvableIndex < processedString.indexOf("?")
  while canProcess do
    val firstSolvableIndex = processedString.indexOf("#?")
    val frontKnown        = processedString.substring(0, firstSolvableIndex + 1)
    val frontDamageGroups = frontKnown.split(raw"\.+").filter(_.nonEmpty)
    val frontGroupIter    = frontDamageGroups.dropRight(1).iterator
    val damageGroupIter   = damageGroupSizes.iterator
    while frontGroupIter.hasNext do
      val (d, len) = (frontGroupIter.next, damageGroupIter.next)
      if d.length != len then
        throw Exception("unsatisfiable: known group sizes don't match")

    def startIndex =
      val i = processedString.substring(0, firstSolvableIndex + 1).reverse
        .indexWhere(_ != '#')
      if i == -1 then 0 else firstSolvableIndex + 1 - i
    val start = startIndex
    var i     = start
    if !damageGroupIter.hasNext then
      throw Exception("unsatisfiable: too many groups")
    val groupSize = damageGroupIter.next
    while i - start < groupSize do
      if i >= processedString.length then
        throw Exception("unsatisfiable: last group too small")
      if processedString(i) == '.' then
        throw Exception(
          "unsatisfiable: encountered . when expecting unknown or #",
        )
      processedString(i) = '#'
      i += 1

    if i < processedString.length then
      if processedString(i) == '#' then
        throw Exception("unsatisfiable: group too large")
      else processedString(i) = '.'

  processedString.mkString

def processDamagesFromBack(damageGroupSizes: Array[Int], s: String) =
  processDamagesFromFront(damageGroupSizes.reverse, s.reverse).reverse

def processDamages(damageGroupSizes: Array[Int], s: String): String =
  processDamagesFromBack(
    damageGroupSizes,
    processDamagesFromFront(damageGroupSizes, s),
  )

def numberOfSpringConfigurations(line: String) =
  val (springs, damageGroupSizes) = parseLine(line)

  def factorial(number: Int): Long = (1 to number).foldLeft(1L)(_ * _)

  def binomial(n: Int, k: Int): Long = ((n - k + 1) to n).foldLeft(1L)(_ * _) /
    factorial(k)

  def weakCompositionsIntoParts(n: Int)(parts: Int): Long =
    binomial(n + parts - 1, n)

  def splitAtIndicesWithoutSeparators(
      seq: Array[Int],
      indices: Array[Int],
  ): Array[Array[Int]] =
    if indices.size == 0 then Array(seq)
    else
      val (first, rest) = seq.splitAt(indices.head)
      first +: splitAtIndicesWithoutSeparators(
        rest.tail,
        indices.tail.map(_ - indices.head),
      )

  def arrangements(line: String, sizes: Array[Int]): Long =
    assert(sizes.forall(_ > 0))
    val result =
      if sizes.sum + sizes.length - 1 > line.length then 0L
      else if sizes.isEmpty then if line.contains('#') then 0L else 1L
      else if line.isEmpty then 0L
      else if line.length == 1 then
        line.head match
          case '#' | '?' if sizes.sameElements(Array(1)) => 1L
          case _                                         => 0L
      else if line.dropWhile(_ == '.').takeWhile(_ != '.').contains('#') &&
        line.dropWhile(_ == '.').takeWhile(_ != '.').length < sizes.head
      then 0
      else if line.forall(_ == '?') then
        weakCompositionsIntoParts(line.length - sizes.sum - sizes.length + 1)(
          sizes.length + 1,
        )
      else if line.count(_ == '#') > sizes.sum then 0L
      else
        val maxSize        = sizes.max
        val maxSizeRegex   = s"#{$maxSize}"
        val maxSizeMatches = maxSizeRegex.r.findAllMatchIn(line)
        if maxSizeMatches.length > sizes.count(_ == maxSize) then 0L
        else if maxSizeMatches.length == sizes.count(_ == maxSize) then
          val substrings = line.split(maxSizeRegex)
          val subSizes = splitAtIndicesWithoutSeparators(
            sizes,
            sizes.zipWithIndex.filter((s, _) => s == maxSize).map((_, i) => i),
          )
          subSizes.d
          substrings.zip(subSizes)
            .map((substring, subsizes) => arrangements(substring, subsizes)).sum
        else
          line.head match
            case '.' => arrangements(line.dropWhile(_ == '.'), sizes)
            case '#' =>
              if line.takeWhile(_ == '#').size > sizes.head then 0L
              else if line.length < sizes.head then 0L
              else
                val (sizeBlock, rest) = line.splitAt(sizes.head)
                if sizeBlock.contains('.') then 0L
                else if rest.nonEmpty && rest.head == '#' then 0L
                else arrangements('.' +: rest.tail, sizes.tail)
            case '?' =>
              if !line.endsWith("?") then
                arrangements(line.reverse, sizes.reverse)
              else
                arrangements('.' +: line.tail, sizes) +
                  arrangements('#' +: line.tail, sizes)

    // (line, sizes, result).d
    result

  arrangements(springs, damageGroupSizes)

def parseLine(line: String) =
  val s"$springs $constraints" = line: @unchecked
  val damageGroupSizes         = constraints.split(',').map(_.toInt)
  (springs, damageGroupSizes)

// part 2
def sumOfUnfoldedSpringConfigurations(input: String) =
  input.linesIterator.zipWithIndex.map((line, i) => (unfold(line), i)).map {
    (line, i) =>
      println(s"at line $i: $line")
      line
  }.map(numberOfSpringConfigurations).sum

def unfold(line: String): String =
  val s"$springs $constraints" = line: @unchecked
  s"${Iterator.continually(springs).take(5).mkString("?")} ${Iterator
      .continually(constraints).take(5).mkString(",")}"
