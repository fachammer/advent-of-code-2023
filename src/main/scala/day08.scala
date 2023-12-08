package day08

// part 1
type DesertMap = Map[String, (String, String)]
def requiredSteps(input: String) =
  val (steps, nodes) = parseInput(input)
  given DesertMap    = nodes

  val (length, _, _) = accumulate((0, "AAA", repeat(steps))):
    (length, node, stepsIter) =>
      Option.when(node != "ZZZ"):
        (length + 1, nextNode(node, stepsIter.next), stepsIter)

  length

// part 2
def requiredStepsAsGhost(input: String) =
  val (steps, nodes) = parseInput(input)
  requiredStepsAsGhostFromParsed(steps, nodes)

def requiredStepsAsGhostFromParsed(steps: String, nodes: DesertMap) =
  given DesertMap = nodes

  case class FindResult(val end: String, val pathLength: Long, val offset: Int)
  def findEndNode(startNode: String, stepOffset: Int) =
    val (end, length, iter) = {
      accumulate((startNode, 0L, repeat(steps.zipWithIndex))):
        case (node, lengthFromStart, stepsWithIndexIter) =>
          Option.when(lengthFromStart == 0 || !node.endsWith("Z")):
            val (step, _) = stepsWithIndexIter.next
            (nextNode(node, step), lengthFromStart + 1, stepsWithIndexIter)
    }
    FindResult(end, length, iter.next._2)

  def cycleLengthStartingFrom(startNode: String) =
    val FindResult(endNode, length, stepsIndex) = findEndNode(startNode, 0)
    assertAllEndNodeCyclesHaveLength(endNode, stepsIndex, length)
    // given the above assertion we know that the cycle length is the same as the initial path length from start to end
    length

  def assertAllEndNodeCyclesHaveLength(end: String, offset: Int, length: Long) =
    accumulate((0L, offset)) { (count, stepOffset) =>
      Option.when(count == 0 || stepOffset != offset):
        val foundNode = findEndNode(end, stepOffset)
        // we assume that there is a unique end node on every cycle
        assert(foundNode.end == end)
        // and that the path lengths from end node to end node are the same regardless of where we are in the step sequence
        assert(foundNode.pathLength == length)
        (count + 1, foundNode.offset)
    }
    true

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Long, b: Long)       = (a * b) / gcd(a, b)

  val startNodes = nodes.filterKeys(_.endsWith("A")).keys
  startNodes.map(cycleLengthStartingFrom).reduceOption(lcm)

def nextNode(node: String, step: Char)(using nodes: DesertMap) = step match
  case 'L' => nodes(node)._1
  case 'R' => nodes(node)._2

def parseInput(input: String): (String, Map[String, (String, String)]) =
  val steps = input.linesIterator.next
  def parseLine(line: String) = line match
    case s"$node = ($left, $right)" => (node, (left, right))
  val nodes = input.linesIterator.drop(2).map(parseLine).toMap
  (steps, nodes)

def repeat[A](seq: Seq[A]) = LazyList.continually(seq).flatten.toIterator

def accumulate[S](initial: S)(op: S => Option[S]): S =
  var a = initial
  while true do
    op(a) match
      case None    => return a
      case Some(b) => a = b
  ???
