package day05

// part 1
case class Almanac(val seeds: Set[Long], val maps: Seq[AlmanacMap])
case class AlmanacMap(
    val source: String,
    val destination: String,
    val mappings: Seq[(Range, Range)]
):
  def apply(number: Long): Long =
    mappings
      .flatMap((source, destination) =>
        if source.contains(number) then
          Some(destination.start + (number - source.start))
        else None
      )
      .headOption
      .getOrElse(number)

  def minOutput: Long = mappings.map(_._2.start).min

  def restrictTo(range: Range): AlmanacMap =
    val maps = for
      (source, destination) <- mappings
      restrictedSource = source.intersectRange(range)
      if !restrictedSource.isEmpty
      restrictedStartIndex = source.indexOf(restrictedSource.start)
    yield (
      restrictedSource,
      Range(destination(restrictedStartIndex), restrictedSource.length)
    )
    AlmanacMap(source, destination, maps)

  def after(map: AlmanacMap): AlmanacMap =
    val newMappings = for
      (beforeSource, beforeDestination) <- map.mappings
      (restrictedSource, restrictedDestination) <- restrictTo(
        beforeDestination
      ).mappings
    yield (
      Range(
        beforeSource(beforeDestination.indexOf(restrictedSource.start)),
        restrictedSource.length
      ),
      restrictedDestination
    )
    AlmanacMapSanitized(map.source, destination, newMappings)

object AlmanacMapSanitized:
  def apply(
      source: String,
      destination: String,
      mappings: Seq[(Range, Range)]
  ): AlmanacMap =
    val sorted = mappings.sortBy((source, _) => source.start)

    if sorted.isEmpty then
      return AlmanacMap(
        source,
        destination,
        Seq((Range(0, Long.MaxValue), Range(0, Long.MaxValue)))
      )

    val minStart = sorted.head._1.start
    val withFirstInterval =
      if minStart > 0 then
        sorted.prepended((Range(0, minStart), Range(0, minStart)))
      else sorted

    val maxEnd = sorted.last._1.end
    val withFirstAndLastInterval =
      if maxEnd < Long.MaxValue
      then
        withFirstInterval.appended(
          (
            Range(maxEnd, Long.MaxValue - maxEnd),
            Range(maxEnd, Long.MaxValue - maxEnd)
          )
        )
      else withFirstInterval

    val pairs =
      withFirstAndLastInterval.zip(withFirstAndLastInterval.drop(1))
    val gaps = pairs.map { (a, b) =>
      val (sourceA, _) = a
      val (sourceB, _) = b
      val gap          = sourceB.start - sourceA.end
      if gap > 0 then Seq((Range(sourceA.end, gap), Range(sourceA.end, gap)))
      else Seq()
    }

    val withGapRanges =
      withFirstAndLastInterval
        .zipAll(gaps, (Range(0, 0), Range(0, 0)), Seq())
        .flatMap((x, y) => y.prepended(x))
    AlmanacMap(source, destination, withGapRanges)

case class Range(start: Long, length: Long):
  def end                    = start + length
  def contains(number: Long) = start <= number && number < end
  def isEmpty                = length <= 0
  def indexOf(number: Long) =
    assert(contains(number))
    number - start

  def apply(index: Long) =
    assert(0 <= index && index < length)
    start + index

  def containsRange(other: Range): Boolean =
    start <= other.start && other.end <= end

  def overlaps(other: Range): Boolean =
    contains(other.start) || contains(other.end)
      || other.contains(start) || other.contains(end)

  def intersectRange(other: Range): Range =
    if overlaps(other) then
      val intersectStart  = start.max(other.start)
      val intersectEnd    = end.min(other.end)
      val intersectLength = intersectEnd - intersectStart
      Range(start.max(other.start), intersectLength)
    else Range(0, 0)

def lowestLocationNumberOfAnySeed(input: String): Long =
  given almanac: Almanac = parseAlmanac(input)
  almanac.seeds.map(locationNumber).min

def parseAlmanac(input: String): Almanac =
  val s"seeds: $seedNumbers" :: mapsStrings = input.split("\n\n").toList
  val seeds = seedNumbers.split(" ").map(_.toLong)

  val maps = parseAlmanacMaps(mapsStrings)
  Almanac(seeds.toSet, maps)

def parseAlmanacMaps(maps: Seq[String]): Seq[AlmanacMap] =
  for case s"$source-to-$destination map:$ranges" <- maps
  yield AlmanacMapSanitized(source, destination, parseRanges(ranges))

def parseRanges(ranges: String): Seq[(Range, Range)] =
  for case s"$destinationStart $sourceStart $rangeLength" <- ranges.linesIterator.toSeq
  yield (
    Range(sourceStart.toLong, rangeLength.toLong),
    Range(destinationStart.toLong, rangeLength.toLong)
  )

def locationNumber(seed: Long)(using almanac: Almanac): Long =
  almanac.maps.foldLeft(seed)((number, map) => map.apply(number))

// part 2
case class AlmanacWithSeedRanges(
    val seedRanges: Seq[Range],
    val maps: Seq[AlmanacMap]
)

def lowestLocationNumberOfAnySeedConsideringRanges(input: String): Long =
  given almanac: AlmanacWithSeedRanges = parseAlmanacWithSeedRanges(input)
  almanac.seedRanges
    .map(composedMap.restrictTo)
    .map(_.minOutput)
    .min

def composedMap(using
    almanac: AlmanacWithSeedRanges
): AlmanacMap =
  val map =
    almanac.maps.reduceLeft((accumulatedMap, map) => map.after(accumulatedMap))

  assert(map.source == "seed")
  assert(map.destination == "location")
  map

def parseAlmanacWithSeedRanges(input: String): AlmanacWithSeedRanges =
  val s"seeds: $seedNumbers" :: mapsStrings = input.split("\n\n").toList
  val seedRanges = seedNumbers
    .split(" ")
    .map(_.toLong)
    .grouped(2)
    .map { case Array(start, len) => Range(start, len) }

  val maps = parseAlmanacMaps(mapsStrings)
  AlmanacWithSeedRanges(seedRanges.toSeq, maps)
