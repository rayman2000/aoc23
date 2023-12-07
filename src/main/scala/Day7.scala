import scala.util.Sorting

case class Hand(hand: String, bid: Int) extends Ordered[Hand] {
  def compare(that: Hand): Int = {
    val a = this.hand
    val b = that.hand
    val rank = handValue(a) - handValue(b)
    if (rank != 0) {
      rank
    } else {
      a.zip(b).map((cs: (Char, Char)) => cardToInt(cs._1) - cardToInt(cs._2)).filter(_ != 0).head
    }
  }

  private def cardToInt(c: Char) = {
    c match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 1
      case 'T' => 10
      case i => i.toString.toInt
    }
  }

  private def handValue(s: String) = {
    if (s.length != 5) {
      throw new Exception("Invalid input")
    }

    var jokers = s

    if(s.contains("J")){
      val noJ = s.replace("J", "")
      val js = s.length - noJ.length
      if(js < 5) {
        val max = noJ.groupBy(identity).maxBy(_._2.length)._1
        jokers = noJ + max.toString * js
      }
    }

    val counts = jokers.groupBy(identity).values.toList.sortBy(_.length).reverse

    counts.length match {
      case 1 => 7
      case 2 => if (counts.head.length == 4) {
        6
      } else {
        5
      }
      case 3 => if (counts.head.length == 3) {
        4
      } else {
        3
      }
      case 4 => 2
      case 5 => 1
      case _ => 0
    }
  }
}

class Day7 extends LoadsInput {

  private val input = loadLines("day7.txt").toArray
  //private val input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483".split("\n")
  private var hands: Array[Hand] = input.map(parseHand)

  def solve1(): Int = {
    Sorting.quickSort(hands)
    hands(0).compare(hands(3))
    hands.indices.map(i => hands(i).bid * (i + 1)).sum
  }

  private def parseHand(s: String) = {
    val parsed = s.split(" ")
    Hand(parsed(0), parsed(1).toInt)
  }

}

