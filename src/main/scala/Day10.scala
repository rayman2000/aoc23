
class Day10 extends LoadsInput {


  private def input = loadWhole("day10.txt").split("\n").map(_.split(""))

  //private def input = ".....\n.S-7.\n.|.|.\n.L-J.\n.....".split("\n").map(_.split(""))

  def solve1(): Int = {

    val karte = input.map(_.map(_ => "."))

    val maxY = input.length
    val maxX = input(0).length

    var marked = 0

    // find S coordinates
    var (row, y) = input.zipWithIndex.filter(_._1.contains("S")).head
    var x = row.indexOf("S")

    karte(y).update(x, "S")

    x += 1
    var dir = 1

    while (input(y)(x) != "S"){
      karte(y).update(x, "P")
      val result = step(x, y, dir)
      x = result._1
      y = result._2
      dir = result._3
    }

    x += 1
    dir = 1
    while(input(y)(x) != "S"){
      // do marking
      var toMark = markPipe(x, y, dir).filter(p => karte(p._2)(p._1) == ".")
      while (toMark.nonEmpty){
        val next = toMark.head
        toMark = toMark.tail
        karte(next._2).update(next._1, "M")
        marked += 1
        toMark = toMark.appendedAll(markAll(next._1, next._2).filter(p => karte(p._2)(p._1) == "."))
      }

      val result = step(x, y, dir)
      x = result._1
      y = result._2
      dir = result._3
    }

    karte.foreach(row => println(row.mkString("")))
    marked

  }

  // dir: 0 from top, 1 from left, 2 from bottom, 3 from right
  def step(x: Int, y: Int, dir: Int): (Int, Int, Int) = {
    val pipe = input(y)(x)
    pipe match {
      case "|" => if (dir == 0) (x, y + 1, 0) else (x, y - 1, 2)
      case "-" => if (dir == 1) (x + 1, y, 1) else (x - 1, y, 3)
      case "L" => if (dir == 0) (x + 1, y, 1) else (x, y - 1, 2)
      case "J" => if (dir == 0) (x - 1, y, 3) else (x, y - 1, 2)
      case "7" => if (dir == 1) (x, y + 1, 0) else (x - 1, y, 3)
      case "F" => if (dir == 2) (x + 1, y, 1) else (x, y + 1, 0)
      case _ => throw new Exception("Invalid pipe")
    }
  }

  def markPipe(x: Int, y: Int, dir: Int): List[(Int, Int)] = {
    val pipe = input(y)(x)
    pipe match {
      case "|" => if (dir == 2) List((x - 1, y)) else List((x + 1, y))
      case "-" => if (dir == 3) List((x, y + 1)) else List((x, y - 1))
      case "L" => if (dir == 3) List((x, y + 1), (x - 1, y)) else List()
      case "J" => if (dir == 1) List() else List((x, y + 1), (x + 1, y))
      case "7" => if (dir == 2) List() else List((x + 1, y), (x, y - 1))
      case "F" => if (dir == 3) List() else List((x - 1, y), (x, y - 1))
      case _ => throw new Exception("Invalid pipe")
    }
  }

  private def markAll(x: Int, y: Int) = {
    List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
  }

}