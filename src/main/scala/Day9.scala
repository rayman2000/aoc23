
class Day9 extends LoadsInput {


  private def input = loadLines("day9.txt")
  //private def input = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45".split("\n").toList

  private def lines = input.map(_.split(" ").iterator.map(_.toInt).toList)



  def solve1(): Int = {
    lines.map(solve).sum
  }

  def solve(line: List[Int]): Int = {
    if (line.filter(_ != 0).isEmpty) {
      0
    } else {
      val diffs = step(line)
      val next = solve(diffs)
      line.head - next
    }
  }

  private def step(curr: List[Int]): List[Int] = {
    curr.sliding(2).map(a => a.last - a.head).toList
  }


}
