import scala.annotation.tailrec

class Day8 extends LoadsInput {

  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
  private def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / gcd(a, b)

  private def lines = loadLines("day8.txt")
  //private def lines = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)".split("\n")

  private val instr = lines.head

  private val nodes = lines.drop(2).map(parseNode).toMap

  private def parseNode(s: String) = {
    val parsed = s.split(" = ")
    val name = parsed(0)
    val children = parsed(1).replace("(", "").replace(")", "").split(", ").toList
    (name, (children.head, children.last))
  }

  def solve1(): Int = {
    solve("AAA")
  }

  private def solve(start: String): Int = {
    var steps: Int = 0
    var pointer: Int = 0
    var next = start
    while (!next.endsWith("Z")) {
      val step = instr.charAt(steps % instr.length)
      val (left, right) = nodes(next)
      next = if (step == 'R') right else left
      steps += 1
      pointer += 1
      if(pointer == instr.length) pointer = 0
    }
    steps
  }

  def solve2(): Int = {
    val starts = nodes.keys.filter(_.endsWith("A"))
    val ends = starts.map(solve).map(BigInt(_))
    ends.reduce(lcm).toInt
  }
}
