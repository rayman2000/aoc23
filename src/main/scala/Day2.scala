class Day2 extends LoadsInput with StringOps {

  private val lines = loadLines("day2.txt")

  def solve1(): Int = {
    var res = 0
    for (i <- lines.indices) {
      val (blue: Int, green: Int, red: Int) = getGameMaxes(i)
      if (blue <= 14 && green <= 13 && red <= 12) {
        res += i + 1
      }
    }
    res
  }

  def solve2(): Int = {
    var res = 0
    for (i <- lines.indices) {
      val (blue: Int, green: Int, red: Int) = getGameMaxes(i)
      res += blue * green * red
    }
    res
  }

  private def getGameMaxes(i: Int): (Int, Int, Int) = {
    val l = lines(i)
    val blue = getMaxCount(l, "blue")
    val green = getMaxCount(l, "green")
    val red = getMaxCount(l, "red")
    (blue, green, red)
  }

  private def getMaxCount(s: String, color: String): Int = {
    var occurrences = s.split(" " + color)
    if (!s.endsWith(color)){
      occurrences = occurrences.dropRight(1)
    }
    occurrences.map(_.split(" ").last.toInt).max
  }
}