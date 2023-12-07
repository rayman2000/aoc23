class Day1 extends LoadsInput with StringOps {

  private val lines = loadLines("day1.txt")


  def solve1(): Int = {
    finish(lines)
  }

  private def finish(input: List[String]): Int = {
    val result = input.map(removeLetters()).map(firstAndLastChar()).map(_.toInt)
    result.sum
  }

  def solve2(): Int = {
    finish(lines.map(replaceNumberWords()))
  }
}