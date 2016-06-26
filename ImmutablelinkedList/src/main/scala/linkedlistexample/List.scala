package linkedlistexample

/**
  * Created by Sukalpo Mitra on 26/6/2016.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T]{
  def isEmpty = true
  // Nothing is a T and Nothing is a subtype of any other type
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Test extends App {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs head
    else nth(n - 1, xs tail)
  }

  println(singleton(1))
  println(singleton(true))

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  println(nth(2, list))
  println(nth(4, list))
  println(nth(-1, list))
}