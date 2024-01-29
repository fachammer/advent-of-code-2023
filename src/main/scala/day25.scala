package day25

import scala.util.Random

// part 1
def productOfThreeCutComponentSizes(input: String) =
  val graph = parseInput(input)
  def contractEdge(
      graph: Map[Set[String], Seq[Set[String]]],
  )(edge: (Set[String], Set[String])): Map[Set[String], Seq[Set[String]]] =
    val (from, to)     = edge
    val fromEdges      = graph(from).filterNot(_ == to)
    val toEdges        = graph(to).filterNot(_ == from)
    val connectedEdges = fromEdges ++ toEdges
    val newEdge        = to ++ from
    val edgeContracted = graph
      .removed(from)
      .removed(to)
      .updated(newEdge, connectedEdges)

    connectedEdges.foldLeft(edgeContracted): (acc, next) =>
      acc.updatedWith(next):
        _.map: xs =>
          xs.map:
            case x if x == to || x == from => newEdge
            case x                         => x

  def choose[A](r: Random)(it: Iterator[A]): A =
    it.zipWithIndex.reduceLeft: (x, y) =>
      if r.nextInt(y._2) == 0 then y else x
    ._1

  def probabilisticMinCut(
      random: Random,
  )(graph: Map[Set[String], Seq[Set[String]]]): (Set[String], Int) =
    if graph.size == 2 then
      val value = graph.iterator.next
      (value._1, value._2.size)
    else
      val edges = graph.toSeq.flatMap((from, tos) => tos.map((from, _)))
      val edge  = choose(random)(edges.iterator)
      probabilisticMinCut(random)(contractEdge(graph)(edge))

  def kargersAlgorithm(random: Random)(
      graph: Map[Set[String], Seq[Set[String]]],
  ): Set[String] =
    while true do
      val (partition, cutSize) = probabilisticMinCut(random)(graph)
      if cutSize == 3 then return partition

    ???

  val random     = Random()
  val multiGraph = graph.map((k, v) => (Set(k), v.map(Set(_)).toSeq))
  probabilisticMinCut(random)(multiGraph)
  val component = kargersAlgorithm(random)(multiGraph)
  component.size * (multiGraph.size - component.size)

def parseInput(input: String) =
  input.linesIterator.toArray.flatMap:
    case s"$from: $tos" =>
      tos.split(' ').flatMap(to => Set((from, to), (to, from)))
  .groupMapReduce((from, _) => from)((_, to) => Set(to))(_ ++ _)
