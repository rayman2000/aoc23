import scala.collection.mutable.ListBuffer

class Day11 extends LoadsInput {

  private def input = loadWhole("day11.txt").split("\n").toList.map(_.split("").toList)
  //private def input = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....".split("\n").toList.map(_.split("").toList)

  def solve1(): Int = {

    input.foreach(row => println(row.mkString("")))
    val (emptyRows, emptyCols) = getEmpties(input)
    val locations = input.zipWithIndex.flatMap{case (row, rowIndex) => row.zipWithIndex.filter{case (col, index) => col == "#"}.map{case(col, index) => (rowIndex, index)}}
    val pairs = locations.combinations(2).toList
    val dists = pairs.map(distance(_, emptyRows, emptyCols))
    //pairs.zipWithIndex.foreach{case (pair, index) => println(s"$pair: ${dists(index)}")}
    val res = dists.sum
    0
  }

  private def distance(points: List[(Int, Int)], emptyRows: List[Int], emptyCols: List[Int]): Long = {

    val expansion: Long = 999999

    val a = points(0)
    val b = points(1)

    val (y1, y2) = if (a._1 > b._1) (a._1, b._1) else (b._1, a._1)
    val (x1, x2) = if (a._2 > b._2) (a._2, b._2) else (b._2, a._2)

    val x = x1 - x2
    val y = y1 - y2

    val rows = emptyRows.filter(row => row > y2 && row < y1).length
    val cols = emptyCols.filter(col => col > x2 && col < x1).length

    Math.abs(x) + Math.abs(y) + rows * expansion + cols * expansion
  }

  def getEmpties(input: List[List[String]]): (List[Int], List[Int]) = {

    val newRows = ListBuffer[Int]()
    val newCols = ListBuffer[Int]()

    val numRow = input.length
    val numCol = input(0).length

    for ((row, index) <- input.zipWithIndex) {
      if (!row.contains("#")) {
        newRows += index
      }
    }
    for (col <- input(0).indices) {
      if (!input.map(_(col)).contains("#")) {
        newCols += col
      }
    }

    (newRows.toList, newCols.toList)
  }
}