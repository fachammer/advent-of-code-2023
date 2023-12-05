package day05

import debug.*
import scala.collection.immutable.NumericRange.Inclusive

// part 1
case class Almanac(val seeds: Set[Long], val maps: Seq[AlmanacMap])
case class AlmanacMap(
    val source: String,
    destination: String,
    val ranges: Seq[(Inclusive[Long], Inclusive[Long])]
):
  def mapRange(number: Long): Long =
    ranges
      .flatMap((source, destination) =>
        if source.contains(number) then
          Some(destination((number - source.start).toInt))
        else None
      )
      .headOption
      .getOrElse(number)

def lowestLocationNumberOfAnySeed(input: String): Long =
  given almanac: Almanac = parseAlmanac(input)
  almanac.seeds.map(locationNumber).min

def parseAlmanac(input: String): Almanac =
  val s"seeds: $seedNumbers" :: mapsStrings = input.split("\n\n").toList
  val seeds = seedNumbers.split(" ").map(_.toLong)

  val maps =
    for case s"$source-to-$destination map:$ranges" <- mapsStrings
    yield AlmanacMap(source, destination, parseRanges(ranges))
  Almanac(seeds.toSet, maps)

def parseRanges(ranges: String): Seq[(Inclusive[Long], Inclusive[Long])] = for
  case s"$destinationStart $sourceStart $rangeLength" <- ranges.linesIterator.toSeq
  sourceRange = sourceStart.toLong to (sourceStart.toLong + rangeLength.toLong)
  destinationRange =
    destinationStart.toLong to (destinationStart.toLong + rangeLength.toLong)
yield (sourceRange, destinationRange)

def locationNumber(seed: Long)(using almanac: Almanac): Long =
  almanac.maps.foldLeft(seed)((number, map) => map.mapRange(number))
