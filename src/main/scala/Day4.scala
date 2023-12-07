import scala.collection.mutable.ListBuffer
import scala.math.{floor, pow}

class Day4 extends LoadsInput {

  //private val games = " Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11".split("\n").toList.map(parseGame)
  private val games = loadLines("day4.txt").map(parseGame)

  private def parseGame(s: String): Game = {
    val winningS = s.split(": ")(1).split('|')(0)
    val ownS = s.split(": ")(1).split('|')(1)
    Game(winningS.split(" ").filter(_ != "").map(_.toInt).toList, ownS.split(" ").filter(_ != "").map(_.toInt).toList)
  }

  def solve1(): Int = {
    games.map((g: Game) => floor(pow(2, g.own.count(g.winning.contains) - 1)).toInt).sum
  }

  def solve2(): Int = {
    var cards = ListBuffer.fill(games.length)(1)
    for (i <- games.indices) {
      val g = games(i)
      val copies = cards(i)
      val wins = g.own.count(g.winning.contains)
      for (j <- 1 to wins) {
        cards(i + j) += copies
      }
    }
    cards.sum
  }
}

private case class Game(winning: List[Int], own: List[Int])