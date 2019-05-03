package lesson8

import scala.collection.JavaConverters._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  // Order(n) : first solution
  // (this solution may consume huge memory to use map for counting elem)
  def solution(a: Array[Int]): Int = {
    val ret = a.toSeq.foldLeft((Map.empty[Int, Int], 0)) {
      case ((container, elemOfMaxCount), x) =>
        val count           = container.getOrElse(x, 0) + 1
        val currentMaxCount = container.getOrElse(elemOfMaxCount, 0)

        val newContainer = container updated (x, count)
        count > currentMaxCount match {
          case true  => (newContainer, x)
          case false => (newContainer, elemOfMaxCount)
        }
    }

    val dominantCount = ret._1.getOrElse(ret._2, 0)
    val firstIndex    = a.zipWithIndex.find(t => t._1 == ret._2).get._2

    dominantCount > a.length / 2 match {
      case true  => firstIndex
      case false => -1
    }
  }

  // idea from reading material
  // Order(n)
  def solution2(a: Array[Int]): Int = {
    val (candidate, _, candidateIndex) =
      a.toSeq.zipWithIndex.foldLeft((0, 0, -1)) {
        case ((candidate, count, candidateIndex), (elem, index)) =>
          count match {
            case 0 => (elem, 1, index)
            case _ =>
              candidate == elem match {
                case true  => (candidate, count + 1, candidateIndex)
                case false => (candidate, count - 1, candidateIndex)
              }
          }
      }

    val count = a.toSeq.count(_ == candidate)
    count > a.length / 2 match {
      case true  => candidateIndex
      case false => -1
    }
  }

  // Order(n * lon(n))
  def solution3(a: Array[Int]): Int = {
    a.length match {
      case 0 => -1
      case 1 => 0
      case _ => {
        val sorted    = a.sorted
        val candidate = sorted(a.length / 2)

        val candidateCount = a.toSeq.count(_ == candidate)

        candidateCount > a.length / 2 match {
          case false => -1
          case true  => a.zipWithIndex.find(t => t._1 == candidate).get._2
        }
      }
    }
  }
}
