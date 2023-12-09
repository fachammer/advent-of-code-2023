package day05

// part 1
case class Almanac(val seeds: Seq[Long], val maps: Seq[AlmanacMap])
case class IntervalMap(
    val domainStart: Long,
    val rangeStart: Long,
    val length: Long,
):
  def domain              = Interval(domainStart, length)
  def range               = Interval(rangeStart, length)
  def apply(number: Long) = domain.indexOf(number).map(range.atIndex)
  def inverse             = IntervalMap(rangeStart, domainStart, length)
  def restrictTo(interval: Interval): IntervalMap =
    val restricted = domain.intersect(interval)
    this(restricted.start).map(IntervalMap(restricted, _))
      .getOrElse(IntervalMap.empty)

object IntervalMap:
  def empty = IntervalMap(0, 0, 0)
  def apply(domain: Interval, rangeStart: Long): IntervalMap =
    IntervalMap(domain.start, rangeStart, domain.length)
  def identity(start: Long, length: Long) = IntervalMap(start, start, length)

case class AlmanacMap(val mappings: Seq[IntervalMap]):
  def apply(number: Long) =
    mappings.flatMap(f => f(number)).headOption.getOrElse(number)

  def minOutput: Long = mappings.map(m => m.range.start).min

  def restrictTo(interval: Interval): AlmanacMap =
    AlmanacMap(mappings.map(_.restrictTo(interval)).filterNot(_.domain.isEmpty))

  def after(map: AlmanacMap): AlmanacMap =
    val newMappings =
      for
        map         <- map.mappings
        restricted  <- restrictTo(map.range).mappings
        domainStart <- map.inverse(restricted.domain.start)
      yield IntervalMap(restricted.range, domainStart).inverse
    AlmanacMap(newMappings).withDomain(Interval(0, Long.MaxValue))

  def withDomain(domain: Interval): AlmanacMap =
    val sortedMappings = mappings.sortBy(m => m.domain.start)
    if sortedMappings.isEmpty then return AlmanacMap(Seq(domain.toIdentityMap))

    val windowedPairs = sortedMappings.zip(sortedMappings.drop(1))
    val gapMaps = windowedPairs.map { (left, right) =>
      val gap = right.domain.start - left.domain.end
      Option.when(gap > 0)(IntervalMap.identity(left.domain.end, gap))
    }
    val filledGaps = sortedMappings.zip(gapMaps :+ Seq.empty)
      .flatMap((interval, gap) => interval +: gap.iterator.toSeq)

    val min = filledGaps.head.domain.start
    val initialInterval = Option
      .when(min > domain.start)(IntervalMap.identity(domain.start, min))
    val max = filledGaps.last.domain.end
    val finalInterval = Option
      .when(max < domain.end)(IntervalMap.identity(max, domain.end - max))
    val mappingsWithEnds = initialInterval.toSeq ++: filledGaps :++
      finalInterval.toSeq

    AlmanacMap(mappingsWithEnds)

case class Interval(start: Long, length: Long):
  def end                    = start + length
  def contains(number: Long) = start <= number && number < end
  def isEmpty                = length <= 0
  def indexOf(number: Long)  = Option.when(contains(number))(number - start)

  def atIndex(index: Long) =
    assert(0 <= index && index < length)
    start + index

  def contains(other: Interval): Boolean =
    start <= other.start && other.end <= end

  def overlaps(other: Interval): Boolean =
    contains(other.start) || contains(other.end) || other.contains(start) ||
      other.contains(end)

  def intersect(other: Interval): Interval =
    val intersectionStart = start.max(other.start)
    val intersectionEnd   = end.min(other.end)
    Interval(intersectionStart, intersectionEnd - intersectionStart)

  def toIdentityMap: IntervalMap = IntervalMap(start, start, length)

def lowestLocationNumberOfAnySeed(input: String): Long =
  given almanac: Almanac = parseAlmanac(input)
  almanac.seeds.map(locationNumber).min

def parseAlmanac(input: String): Almanac =
  val s"seeds: $numbers" :: mapsStrings = input.split("\n\n").toList: @unchecked
  val seeds = numbers.split(" ").toIndexedSeq.map(_.toLong)
  val maps  = parseAlmanacMaps(mapsStrings)
  Almanac(seeds, maps)

def parseAlmanacMaps(maps: Seq[String]): Seq[AlmanacMap] =
  for case s"$source-to-$destination map:$intervals" <- maps
  yield AlmanacMap(parseIntervals(intervals))
    .withDomain(Interval(0, Long.MaxValue))

def parseIntervals(intervals: String): Seq[IntervalMap] =
  for case s"$rangeStart $domainStart $length" <- intervals.linesIterator.toSeq
  yield IntervalMap(domainStart.toLong, rangeStart.toLong, length.toLong)

def locationNumber(seed: Long)(using almanac: Almanac): Long =
  almanac.maps.foldLeft(seed)((number, map) => map.apply(number))

// part 2
case class AlmanacWithSeedIntervals(
    val seedIntervals: Seq[Interval],
    val maps: Seq[AlmanacMap],
):
  def apply(interval: Interval): Seq[Interval] =
    maps.reduce((accumulatedMap, map) => map.after(accumulatedMap))
      .restrictTo(interval).mappings.map(_.range)

  def lowestLocationNumber = seedIntervals.map(this(_).map(_.start).min).min

def lowestLocationNumberOfAnySeedWithRangeInput(input: String): Long =
  parseAlmanacWithRangeInput(input).lowestLocationNumber

def parseAlmanacWithRangeInput(input: String): AlmanacWithSeedIntervals =
  val almanac = parseAlmanac(input)
  val intervals = almanac.seeds.grouped(2).map { case Seq(start, len) =>
    Interval(start, len)
  }
  AlmanacWithSeedIntervals(intervals.toSeq, almanac.maps)
