package classexample

/**
  * Created by Sukalpo Mitra on 25/6/2016.
  */

object intSets extends App {
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  println(t1)
  println(t2)
  println(t1 union t2)
}

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains (x: Int):Boolean = false
  def incl (x:Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."

}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x:Int): IntSet = {
    if (x < elem) left incl x
    else if (x > elem) right incl x
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}