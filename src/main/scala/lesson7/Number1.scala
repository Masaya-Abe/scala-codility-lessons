package lesson7

import scala.collection.JavaConverters._
import scala.collection.mutable.Stack

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  private def isSymmetric(char: Char, char2: Char): Boolean = {
    char match {
      case '(' if char2 == ')' => true
      case '{' if char2 == '}' => true
      case '[' if char2 == ']' => true
      case _                   => false
    }
  }

  def solution(s: String): Int = {
    // write your code in Scala 2.12

    val stack = Stack.empty[Char]
    s.toSeq.foreach(char => {
      stack.isEmpty match {
        case true => stack.push(char)
        case false =>
          char match {
            case x @ ('(' | '{' | '[')                              => stack.push(x)
            case x @ (')' | '}' | ']') if isSymmetric(stack.top, x) => stack.pop
            case _                                                  => Unit
          }
      }
    })

    stack.size match {
      case 0 => 1
      case _ => 0
    }
  }
}
