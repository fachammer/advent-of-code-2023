package day12

// part 1
def sumOfSpringConfigurations(input: String) =
  input.linesIterator.map(numberOfSpringConfigurations).sum

def numberOfSpringConfigurations(line: String) =
  val s"$springs $constraints" = line: @unchecked
  val damageGroupSizes         = constraints.split(',').map(_.toInt)

  import scala.collection.mutable
  val queue     = mutable.Queue(springs)
  val satisfied = mutable.Stack[String]()
  def isValid(springs: String) =
    springs.split(raw"\.+").map(_.length).filter(_ != 0)
      .sameElements(damageGroupSizes)

  while !queue.isEmpty do
    val s = queue.dequeue()
    if s.contains('?') then
      val withWorking = s.replaceFirst(raw"\?", ".")
      val withDamaged = s.replaceFirst(raw"\?", "#")
      queue.enqueueAll(Seq(withWorking, withDamaged))
    else if isValid(s) then satisfied.push(s)

  satisfied.length
