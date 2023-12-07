import scala.collection.mutable.ListBuffer

class Day3 extends LoadsInput {

  private val lines = loadLines("day3.txt")
  //private val lines = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..".split("\n").toList

  def solve1(): Int = {
    var res = 0
    var prevSymbols = List[Int]()
    var currentSymbols = symbolIndices(lines.head)
    for (i <- lines.indices) {
      var nextSymbols = List[Int]()
      if(i < lines.length - 1) {
        nextSymbols = symbolIndices(lines(i + 1))
      }
      for (n <- numberIndices(lines(i))) {
        var isPart = false
        if(currentSymbols.contains(n.start - 1) || currentSymbols.contains(n.end)){
          isPart = true
        } else {
          for (i <- n.start -1 until n.end + 1){
            if(prevSymbols.contains(i) || nextSymbols.contains(i)){
              isPart = true
            }
          }
        }
        if (isPart) {
          res += n.value
        }
      }
      prevSymbols = currentSymbols
      currentSymbols = nextSymbols
    }
    res
  }

  def solve2(): Int = {
    var res = 0
    var prevNumbers = List[Number]()
    var currentNumbers = numberIndices(lines.head)
    for (i <- lines.indices) {
      var nextNumbers = List[Number]()
      if (i < lines.length - 1) {
        nextNumbers = numberIndices(lines(i + 1))
      }
      for (g <- gearIndices(lines(i))) {
        var adjacent = ListBuffer[Int]()

        for (n <- prevNumbers) {
          if(n.start - 1 <= g && n.end >= g){
            adjacent.append(n.value)
          }
        }
        for (n <- nextNumbers) {
          if (n.start - 1 <= g && n.end >= g) {
            adjacent.append(n.value)
          }
        }
        for (n <- currentNumbers) {
          if (g == n.start - 1 || g == n.end) {
            adjacent.append(n.value)
          }
        }
        if (adjacent.length == 2) {
          res += adjacent.head * adjacent(1)
        }
      }
      prevNumbers = currentNumbers
      currentNumbers = nextNumbers
    }
    res

  }

  private def numberIndices(s: String): List[Number] = {
    val digitIndices = s.indices.filter(s.charAt(_).isDigit).toList
    var res = ListBuffer[Number]()
    var start = -1
    var end = -1

    for (i <- digitIndices) {

      // Start parsing a new number
      if (start == -1) {
        start = i
        end = i + 1
      } else {
        // Continue parsing the current number
        if(i == end){
          end += 1
        } else{
          res.addOne(Number(s.substring(start, end).toInt, start, end))
          start = i
          end = i + 1
        }
      }
    }
    if(start != -1){
      res.addOne(Number(s.substring(start, end).toInt, start, end))
    }
    res.toList
  }

  private def symbolIndices(s: String): List[Int] = {
    s.indices.filter((i : Int) => s.charAt(i) != '.' && !s.charAt(i).isDigit).toList
  }

  private def gearIndices(s: String): List[Int] = {
    s.indices.filter(s.charAt(_) == '*').toList
  }

  private case class Number(value: Int, start: Int, end: Int)

}
