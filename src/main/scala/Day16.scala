class Day16 extends LoadsInput with MatrixOps {

  private def doStep(mirrors: List[List[Char]], beams: List[List[String]], x: Int, y: Int, dir: Int): List[List[String]] = {
    //printMatrix(beams)

    if(y < 0 || mirrors.length <= y || x < 0 || mirrors(y).length <= x){
      beams
    } else{
      val beam = beams(y)(x)
      if (beam.contains(dir.toString)) {
        beams
      } else{
        val updatedBeams = beams.updated(y, beams(y).updated(x, beam + dir.toString))
        val mirror = mirrors(y)(x)
        (mirror, dir) match {
          case ('.', 0) => doStep(mirrors, updatedBeams, x, y+1, 0)
          case ('.', 1) => doStep(mirrors, updatedBeams, x+1, y, 1)
          case ('.', 2) => doStep(mirrors, updatedBeams, x, y-1, 2)
          case ('.', 3) => doStep(mirrors, updatedBeams, x-1, y, 3)
          case ('/', 0) => doStep(mirrors, updatedBeams, x-1, y, 3)
          case ('/', 1) => doStep(mirrors, updatedBeams, x, y-1, 2)
          case ('/', 2) => doStep(mirrors, updatedBeams, x+1, y, 1)
          case ('/', 3) => doStep(mirrors, updatedBeams, x, y+1, 0)
          case ('\\', 0) => doStep(mirrors, updatedBeams, x+1, y, 1)
          case ('\\', 1) => doStep(mirrors, updatedBeams, x, y+1, 0)
          case ('\\', 2) => doStep(mirrors, updatedBeams, x-1, y, 3)
          case ('\\', 3) => doStep(mirrors, updatedBeams, x, y-1, 2)
          case ('|', 0) => doStep(mirrors, updatedBeams, x, y+1, 0)
          case ('|', 1) => doStep(mirrors, doStep(mirrors, updatedBeams, x, y-1, 2), x, y+1, 0)
          case ('|', 3) => doStep(mirrors, doStep(mirrors, updatedBeams, x, y-1, 2), x, y+1, 0)
          case ('|', 2) => doStep(mirrors, updatedBeams, x, y-1, 2)
          case ('-', 1) => doStep(mirrors, updatedBeams, x+1, y, 1)
          case ('-', 3) => doStep(mirrors, updatedBeams, x-1, y, 3)
          case ('-', 0) => doStep(mirrors, doStep(mirrors, updatedBeams, x-1, y, 3), x+1, y, 1)
          case ('-', 2) => doStep(mirrors, doStep(mirrors, updatedBeams, x-1, y, 3), x+1, y, 1)
        }
      }
    }
  }

  def solve1(): Int = {
    val mirrors = loadWhole("day16.txt").split("\n").map(_.toList).toList
    val beams = mirrors.map(_.map(_ => ""))
    val entries = mirrors.indices.flatMap(y => List((0, y, 1), (mirrors(y).length - 1, y, 3))) ++
      mirrors(0).indices.flatMap(x => List((x, 0, 0), (x, mirrors.length - 1, 2)))

    entries.map(e => doStep(mirrors, beams, e._1, e._2, e._3).flatten.count(_.nonEmpty)).max

  }
}
