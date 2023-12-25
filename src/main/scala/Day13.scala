import scala.math.Ordering.Implicits.infixOrderingOps

class Day13 extends LoadsInput {

  private def input = loadWhole("day13.txt").split("\n\n").map(parseNote)


  private def parseNote(in: String): (List[String], List[String]) = {
    val raw = in.split("\n").map(_.split("").toList).toList
    val rows = raw.map(_.foldLeft("")((acc, i) => acc + i))
    val cols = raw.transpose.map(_.foldLeft("")((acc, i) => acc + i))
    (rows, cols)
  }

  private def stringDiffs(a: String, b: String): Int = {
    a.zipWithIndex.filter{case (c, i) => c != b(i)}.length
  }

  private def solve(rows: List[String]): Int = {
    var found = true
    for(i <- 1 until rows.length){
      val end = if (i < rows.length - i) i else rows.length - i
      for (j <- 0 until end) {
        if (found) {
          val left = i - 1 - j
          val right = i + j
          if (rows(left) != rows(right)) {
            return i
          }
        }
      }
    }
    0
  }

  private def solve2(rows: List[String]): Int = {
    //val original = solve(rows)

    for(i <- 1 until rows.length){
      var mismatch = 0
      val end = if (i < rows.length - i) i else rows.length - i
      for (j <- 0 until end) {
        if (mismatch < 2) {
          val left = i - 1 - j
          val right = i + j
          mismatch += stringDiffs(rows(left), rows(right))
        }
      }
      if(mismatch == 1) {
        return i
      }
    }
    0
  }

  private def checkBoth(rows: List[String], cols: List[String]): Int = {
    var row = solve2(rows)
    if (row > 0) {
      100*row
    } else {
      solve2(cols)
    }
  }

  private def smudge(rows: List[String]): Int = {
    val original = solve(rows)

    for (i <- rows.indices) {
      for (j <- rows(i).indices) {
        val newSymbol = if (rows(i)(j) == '.') '#' else '.'
        val smudged = rows.updated(i, rows(i).updated(j, newSymbol))
        val res = solve(smudged)
        if (res != 0 && res != original) {
          println(s"Found $res at $i, $j")
          println(s"Original ($original)")
          rows.foreach(println)
          println("Smudged")
          smudged.foreach(println)
          println("")
          return res
        }
      }
    }
    0
  }

  def solve1(): Int = {
    input.map(t => checkBoth(t._1, t._2)).sum
  }

}
