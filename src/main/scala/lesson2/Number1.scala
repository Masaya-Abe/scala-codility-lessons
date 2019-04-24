package lesson2

object Solution {
  def solution(a: Array[Int]): Int = {
    val map = collection.mutable.Map.empty[Int, Int]
    a.foreach(x => {
      map.get(x) match {
        case Some(count) => map.put(x, count + 1)
        case None        => map.put(x, 1)
      }
    })

    map.find(element => element._2 % 2 == 1).map(_._1).get
  }
}
