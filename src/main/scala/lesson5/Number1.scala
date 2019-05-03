import scala.collection.JavaConverters._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  // order : n ^ 2
  // result : timeout
  def solution(a: Array[Int]): Int = {
    val seq = a.toSeq.zipWithIndex
    val ret = seq
      .filter { case (x, i) => x == 0 }
      .flatMap {
        case (x, i) => {
          seq
            .drop(i + 1)
            .filter(_._1 == 1)
            .map { case (_, j) => (i, j) }
        }
      }

    ret.length
  }

  // order : n
  // result : ok
  def solution2(a: Array[Int]): Int = {
    val seq = a.toSeq
    val ret = seq.foldLeft((0, 0L)) {
      case ((eastCount, passingCount), car) => {
        car match {
          case 0 => (eastCount + 1, passingCount)
          case 1 => (eastCount, passingCount + eastCount)
        }
      }
    }

    ret._2 match {
      case x if x > 1000000000 => -1
      case x                   => x.toInt
    }
  }
}
