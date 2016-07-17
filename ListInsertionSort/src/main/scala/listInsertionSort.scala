import scala.collection.immutable.::

/**
  * Created by Sukalpo Mitra on 17/7/2016.
  */
object listInsertionSort {

  def isort(xs: List[Int]) : List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  //The worst case complexity of this algo is N*N. That is if N is the length of list N times insert and n times sort

}
