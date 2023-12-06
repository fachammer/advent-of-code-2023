package day06

import java.math.MathContext
import java.math.RoundingMode

// part 1
def productOfNumberOfWaysToBeatRecord(input: String) =
  val List(s"Time: $timesString", s"Distance: $distanceString") =
    input.linesIterator.toSeq: @unchecked

  val times =
    timesString.split(" +").map(_.strip).filterNot(_.isEmpty).map(_.toInt)
  val distances =
    distanceString.split(" +").map(_.strip).filterNot(_.isEmpty).map(_.toInt)
  val races = times.zip(distances)

  // let t_r be the duration of the race
  // let t_s be the duration of holding the button
  // let d(t_s, t_r) be the distance travelled after t_r seconds when holding the button for t_s seconds
  // d(t_s) = t_s(t_r-t_s)
  // let r be the record distance
  // we want to find all the choices for t_s such that d(t_s, t_r) > r
  // i.e. all t_d such that t_s (t_r - t_s) - r > 0
  // we look for the roots of -t_s^2 + t_r t_s -r
  // those are (-1 / 2) (-t_r +- sqrt(t_r^2 - 4r))

  def roots(raceDuration: Int, recordDistance: Int): (Double, Double) =
    (
      (-1.0 / 2) * (-raceDuration + scala.math.sqrt(
        raceDuration * raceDuration - 4 * recordDistance
      )),
      (-1.0 / 2) * (-raceDuration - scala.math.sqrt(
        raceDuration * raceDuration - 4 * recordDistance
      ))
    )

  races
    .map(roots.tupled)
    .map((left, right) => (left.nextLong, right.previousLong))
    .map((left, right) => {
      assert(right >= left)
      right - left + 1
    })
    .product

// part 2
def numberOfWaysToBeatRecord(input: String) =
  val List(s"Time: $timesString", s"Distance: $distanceString") =
    input.linesIterator.toSeq: @unchecked

  val time     = timesString.replaceAll(" ", "").toLong
  val distance = distanceString.replaceAll(" ", "").toLong

  def bigDecimalRoots(
      raceDuration: Long,
      recordDistance: Long
  ): (BigDecimal, BigDecimal) =
    (
      BigDecimal(-1.0 / 2) * (BigDecimal(-raceDuration) + BigDecimal(
        raceDuration * raceDuration - 4 * recordDistance
      ).bigDecimal.sqrt(MathContext.DECIMAL128)),
      BigDecimal(-1.0 / 2) * (BigDecimal(-raceDuration) - BigDecimal(
        raceDuration * raceDuration - 4 * recordDistance
      ).bigDecimal.sqrt(MathContext.DECIMAL128))
    )

  val (left, right) = bigDecimalRoots(time, distance)

  right.previousLong - left.nextLong + 1

extension (number: BigDecimal)
  def nextLong =
    if number.isValidLong then number.longValue + 1
    else
      val digitsBeforeDecimalPoint =
        number.bigDecimal.toString.split(raw"\.").head.length
      number
        .round(MathContext(digitsBeforeDecimalPoint, RoundingMode.CEILING))
        .longValue
  def previousLong =
    if number.isValidLong then number.longValue - 1
    else
      val digitsBeforeDecimalPoint =
        number.bigDecimal.toString.split(raw"\.").head.length
      number
        .round(MathContext(digitsBeforeDecimalPoint, RoundingMode.FLOOR))
        .longValue
