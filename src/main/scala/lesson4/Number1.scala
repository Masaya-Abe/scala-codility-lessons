package lesson4

object Solution {
  def solution(a: Array[Int]): Int = {
    val ret = a.toSeq.foldLeft((Int.MaxValue, 0, Set.empty[Int])) {
      case ((min, max, set), x) => {
        val newMin = x < min match {
          case true  => x
          case false => min
        }
        val newMax = x > max match {
          case true  => x
          case false => max
        }

        (newMin, newMax, set + x)
      }
    }

    ret._1 == 1 && ret._2 == a.length && ret._3.size == a.length match {
      case true  => 1
      case false => 0
    }
  }
}
