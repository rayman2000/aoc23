import scala.collection.{SeqLike, SeqOps}
import scala.io.Source

object AdventOfCode {
  def main(args: Array[String]): Unit = {
    val solver = new Day16()
    println(solver.solve1())
  }
}

trait LoadsInput {
  def loadLines(file: String): List[String] = {
    val bufferedSource = Source.fromFile(s"src/main/resources/$file")
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  def loadWhole(file: String): String = {
    val bufferedSource = Source.fromFile(s"src/main/resources/$file")
    val res = bufferedSource.mkString
    bufferedSource.close
    res
  }
}

trait StringOps {
  def removeLetters(): String => String = (s: String) => s.filter("1234567890".contains(_))

  def replaceNumberWords(): String => String = (s: String) => s.replace("one", "on1e")
                                                            .replace("two", "tw2o")
                                                            .replace("three", "thr3ee")
                                                            .replace("four", "fo4ur")
                                                            .replace("five", "fi5ve")
                                                            .replace("six", "si6x")
                                                            .replace("seven", "sev7en")
                                                            .replace("eight", "ei8ght")
                                                            .replace("nine", "ni9ne")

  def firstAndLastChar(): String => String = (s: String) => s"${s(0)}${s.last}"

}

trait MatrixOps {

  def matrixToString(m: Seq[Seq[String]]): String = {
    m.map(_.mkString("")).mkString("\n")
  }
  def printMatrix(m: Seq[Seq[String]]) = {
    println(matrixToString(m))
  }
}