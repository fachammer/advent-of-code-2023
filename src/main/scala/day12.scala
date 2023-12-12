package day12

import scala.util.Try

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

  import scala.collection.mutable
  val stack     = mutable.Stack(springs)
  val satisfied = mutable.Stack[String]()

  def isDefinitelyValid(springs: String) =
    !springs.contains("?") && springs.split(raw"\.+").map(_.length)
      .filter(_ != 0).sameElements(damageGroupSizes)

  while !stack.isEmpty do
    val s = Try(processDamages(damageGroupSizes, stack.pop))
    if !s.isFailure then
      s.get match
        case s if s.contains('?') =>
          val withWorking = s.replaceFirst(raw"\?", ".")
          val withDamaged = s.replaceFirst(raw"\?", "#")
          stack.push(withWorking, withDamaged)
        case s if isDefinitelyValid(s) => satisfied.push(s)
        case _                         =>

  satisfied.length

def parseLine(line: String) =
  val s"$springs $constraints" = line: @unchecked
  val damageGroupSizes         = constraints.split(',').map(_.toInt)
  (springs, damageGroupSizes)

// part 2
def sumOfUnfoldedSpringConfigurations(input: String) =
  input.linesIterator.zipWithIndex
    .tapEach((line, i) => println(s"at line $i: $line"))
    .map((line, _) => unfold(line)).map(numberOfSpringConfigurations).sum

def unfold(line: String): String =
  val s"$springs $constraints" = line: @unchecked
  s"${Iterator.continually(springs).take(5).mkString("?")} ${Iterator
      .continually(constraints).take(5).mkString(",")}"
