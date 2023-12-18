import scala.collection.mutable

case class Line(content: String, info: List[Int])

class Day12 extends LoadsInput {

  private def input = loadLines("day12.txt")
  //= List(".?.?.#?#??.?????? 4,2")

  private def parsed = input.map(parseLine)

  private val cache = mutable.Map.empty[String, Long]


  private def parseLine(in: String): Line = {
    val content = in.split(" ").toList
    //val info = content(1).split(",").map(_.toInt).toList
    //Line(content(0), info)
    val line = ("?" + content(0)).repeat(5).tail
    val info = ("," + content(1)).repeat(5).tail.split(",").map(_.toInt).toList
    Line(line, info)
  }

  def solve1(): Int = {
    val sols = parsed.map(l => solveLine(l.content, l.info))
    val res = sols.sum
    println(res)
    res.toInt
  }

  private def toKey(line: String, info: List[Int]): String = {
    s"$line${info.mkString(",")}"
  }


  private def solveLine(line: String, info: List[Int]): Long = {
    if (line.isEmpty && info.isEmpty) {
      return 1
    }
    if (line.isEmpty) {
      return 0
    }
    if (info.isEmpty) {
      return if(line.contains("#")) 0 else 1
    }

    val key = toKey(line, info)
    if (cache.contains(key)) {
      cache(key)
    } else {
      val char = line.head
      var hashCon: Long = 0
      var dotCon: Long = 0

      if (char == '.' || char == '?') {
        dotCon = solveLine(line.drop(1), info)
      }

      if (char == '#' || char == '?') {
        if (line.length >= info.head) {
          val chunk = line.take(info.head)
          if (!chunk.contains(".") && !(line.length > chunk.length && line(chunk.length) == '#')) {
            hashCon = solveLine(line.drop(info.head + 1), info.tail)
          }
        }
      }
      val res = dotCon + hashCon
      cache += (key -> res)
      res
    }
  }
}
