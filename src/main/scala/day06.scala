package day06

// part 1
def productOfNumberOfWaysToBeatRecord(input: String) =
  val timePattern     = "Time: +(.*)".r
  val distancePattern = "Distance: +(.*)".r
  val Seq(
    timePattern(durationsString),
    distancePattern(recordDistancesString)
  ) = input.linesIterator.toSeq: @unchecked

  val raceDurations   = durationsString.split(" +").map(_.toLong)
  val recordDistances = recordDistancesString.split(" +").map(_.toLong)
  val races           = raceDurations.zip(recordDistances)

  races.map(numberOfWaysToBeatRecord.tupled).product

def numberOfWaysToBeatRecord(raceDuration: Long, recordDistance: Long): Long =
  val discriminant = raceDuration * raceDuration - 4 * recordDistance
  val leftRoot  = 0.5 * (raceDuration - scala.math.sqrt(discriminant.toDouble))
  val rightRoot = 0.5 * (raceDuration + scala.math.sqrt(discriminant.toDouble))
  rightRoot.previousLong - leftRoot.nextLong + 1

// part 2
def numberOfWaysToBeatRecord(input: String): Long =
  val Seq(s"Time: $timesString", s"Distance: $distanceString") =
    input.linesIterator.toSeq: @unchecked

  val raceDuration   = timesString.replaceAll(" ", "").toLong
  val recordDistance = distanceString.replaceAll(" ", "").toLong

  numberOfWaysToBeatRecord(raceDuration, recordDistance)

extension (number: Double)
  def nextLong     = number.floor.toLong + 1
  def previousLong = number.ceil.toLong - 1
