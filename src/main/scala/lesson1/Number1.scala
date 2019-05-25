package lesson1

object Number1 {
  def solution(n: Int): Int = {
    val idxOfOne = n.toBinaryString.zipWithIndex
      .filter { case (c, i) => c == '1' }
      .map { case (c, i) => i }
      .toSeq

    idxOfOne.length match {
      case i if i <= 1 => 0
      case _ => {
        idxOfOne.sliding(2).map(slide => slide(1) - slide(0) - 1).max
      }
    }
  }

  def solution2(n: Int): Int = {
    val (gap, max) = n.toBinaryString.foldLeft((0, 0)) {
      case ((gap, max), c) => {
        c match {
          case '0' => (gap + 1, max)
          case '1' =>
            gap > max match {
              case true  => (0, gap)
              case false => (0, max)
            }
        }
      }
    }

    max
  }
}
