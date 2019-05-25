package lesson10

import scala.math.sqrt

object Number1 {
  def solution(n: Int): Int = {
    val sqrtN = sqrt(n).toInt

    (1 to sqrtN)
      .filter(n % _ == 0)
      .flatMap(x => Seq(x, n / x))
      .distinct
      .length
  }
}
