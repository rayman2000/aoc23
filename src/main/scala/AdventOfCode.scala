import scala.io.Source

object AdventOfCode {
  def main(args: Array[String]): Unit = {
    val solver = new Day4()
    println(solver.solve2())
  }
}

trait LoadsInput {
  def load(file: String): List[String] = {
    println("Loading input")
    val bufferedSource = Source.fromFile(s"src/main/resources/$file")
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
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