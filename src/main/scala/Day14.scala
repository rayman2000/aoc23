

class Day14 extends LoadsInput with MatrixOps {

  private def input = loadWhole("day14.txt").split("\n").map(_.split(""))


  private def tiltColumn(board: Array[Array[String]], north: Boolean): Array[Array[String]] = {
    val iRange = if (north) 1 until board.length else (board.length - 2) to 0 by -1
    val offset = if (north) -1 else 1
    val end = if (north) 0 else board.length - 1

    for (i <- iRange) {
      for (j <- 0 until board(i).length) {
        if (board(i)(j) == "O") {
          var current_index = i
          while (current_index != end && board(current_index + offset)(j) == ".") {
            board.update(current_index + offset, board(current_index + offset).updated(j, "O"))
            board.update(current_index, board(current_index).updated(j, "."))
            current_index += offset
          }
        }
      }
    }
    board
  }

  private def tiltRow(board: Array[Array[String]], west: Boolean): Array[Array[String]] = {
    val jRange = if (west) 1 until board(0).length else (board(0).length - 2) to 0 by -1
    val offset = if (west) -1 else 1
    val end = if (west) 0 else board(0).length - 1

    for (i <- 0 until board.length) {
      for (j <- jRange) {
        if (board(i)(j) == "O") {
          var current_index = j
          while (current_index != end && board(i)(current_index + offset) == ".") {
            board.update(i, board(i).updated(current_index + offset, "O"))
            board.update(i, board(i).updated(current_index, "."))
            current_index += offset
          }
        }
      }
    }
    board
  }

  private def count(board: Array[Array[String]]): Int = {
    board.reverse.zipWithIndex.map{case (row, i) => row.count(_ == "O") * (i+1)}.sum
  }

  private def rotate(board: Array[Array[String]]): Array[Array[String]] = {
    val res = board.clone()
    //println("Tilting North")
    tiltColumn(res, true)
    //printMatrix(res)
    //println()
    //println("Tilting West")
    tiltRow(res, true)
    //printMatrix(res)
    //println()
    //println("Tilting South")
    tiltColumn(res, false)
    //printMatrix(res)
    //println()
    //println("Tilting East")
    tiltRow(res, false)
    //printMatrix(res)
    //println()
    res
  }



  def solve1(): Int = {

    var cycles: Array[String] = Array()

    cycles :+= matrixToString(input.map(_.toList))

    var previous = input.clone()
    var current = rotate(previous)
    //printMatrix(current)
    while(!cycles.contains(matrixToString(current.map(_.toList)))){
      cycles :+= matrixToString(current.map(_.toList))
      previous = current
      current = rotate(previous)
      //printMatrix(current)
    }

    val cycleStart = cycles.indexOf(matrixToString(current.map(_.toList)))
    val states = cycles.slice(cycleStart, cycles.length)

    val its = 1000000000 - cycleStart

    val finalState = states(its % states.length)

    val result = count(finalState.split("\n").map(_.split("")))

    result
  }
}
