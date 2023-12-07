case class Range(destination: Long, source: Long, length: Long)

case class Mapper(name: String, ranges: List[Range]) {

  def map(i: Long): Long = {
    var matchingRange = ranges.filter((r: Range) => r.source <= i && i < r.source + r.length)
    if (matchingRange.length > 1) {
      throw new Exception("Invalid input")
    }
    if(matchingRange.isEmpty){
      i
    } else {
      i + matchingRange.head.destination - matchingRange.head.source
    }
  }
}

class Day5 extends LoadsInput {

  private val input = loadWhole("day5.txt").split("\n\n")
  private val seeds = input.head.drop(7)
  private val mappers = input.drop(1).map(parseMapper)

  private def parseSeeds1(s: String): List[Long] = s.split(" ").map(_.toLong).toList

  private def parseSeeds2(s: String): Iterator[Long] = {
    s.split(" ").map(_.toLong).grouped(2).flatMap {
      case Array(start, len) => start until (start + len)
    }
  }

  private def parseMapper(s: String): Mapper = {
    val parts = s.split("\n").toList
    Mapper(parts.head, parts.drop(1).map(parseRange))
  }

  private def parseRange(s: String): Range = {
    val parts = s.split(" ")
    Range(parts(0).toLong, parts(1).toLong, parts(2).toLong)
  }

  def solve1(): Long = {
    val seeds1 = parseSeeds1(seeds)
    seeds1.map(mapSeed).min
  }

  def solve2(): Long = {
    parseSeeds2(seeds).map(mapSeed).min
  }

  private def mapSeed(seed: Long): Long = {
    mappers.foldLeft(seed)((i: Long, m: Mapper) => m.map(i))
  }

}
