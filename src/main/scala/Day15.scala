

case class Step(label: String, operation: String)

class Day15 extends LoadsInput {

  private def input = loadWhole("day15.txt").replace("\n", "").split(",").map(parseStep).toList

  private def parseStep(s: String): Step = {
    if (s.contains("-")){
      Step(s.dropRight(1), "-")
    } else {
      val parsed = s.split("=")
      Step(parsed(0), parsed(1))
    }
  }

  private def hashChar(currentHash: Int, char: Char): Int = {
    (char.toInt + currentHash) * 17 % 256
  }

  private def hashString(s: String): Int = {
    s.foldLeft(0)(hashChar)
  }

  private def doStep(boxes: List[List[Step]], step: Step): List[List[Step]] = {
    val hsh = hashString(step.label)
    val oldBox = boxes(hsh)
    val newBox = {
      if (step.operation == "-") {
        oldBox.filterNot(_.label == step.label)
      } else if (oldBox.filter(_.label == step.label).isEmpty) {
        oldBox :+ step
      } else {
        oldBox.map(s => if (s.label == step.label) Step(s.label, step.operation) else s)
      }
    }
    boxes.updated(hsh, newBox)
  }

  def solve1(): Int = {
    val boxes: List[List[Step]] = List.fill(256)(List())
    val lenses = input.foldLeft(boxes)(doStep)
    lenses.zipWithIndex.flatMap{ case (box, boxIndex) => box.zipWithIndex.map{ case (lens, lensIndex) => (boxIndex + 1)*(lensIndex + 1)*lens.operation.toInt }}.sum
  }

}
