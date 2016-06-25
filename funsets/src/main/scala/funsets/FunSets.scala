package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets extends App {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean
  /**
    * Is basically saying "there is a type Set that is defined as a function that takes an integer in as a parameter
    * and returns a boolean". The contains function takes in a Set and an integer and simply passes that integer in as
    * a parameter to the set (which is a function).
    */

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  /**
    * This is defining a function singletonSet that takes in a an integer and returns a function that takes in an
    * integer and compares it to the integer originally passed into singletonSet. Since this function takes in an int
    * and returns a bool it satisfies the definition of Set from above and the return type of singletonSet can be
    * declared to be of the type Set. So for a quick example:
    * def set = singletonSet(5)
    * val inSet = contains(set, 6)
    */


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)

  /**
    * val isEven = (x: Int) => x % 2 == 0
    *isEven(20) // true
    *
    *val s1 = singletonSet(1)
    *val s2 = singletonSet(2)
    *filter(s1, isEven)(1) // false
    *filter(s2, isEven)(2) // true
    */


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
  /*def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (contains(s, a) && forall( singletonSet(a),p)) true
      else iter(a + 1)
    }
    iter(-bound)
  }*/

  /**
    * def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
    * This one is tricky. The solution means not all elements in s satisfies !p,
    * which in turn indicates there is at least one element in s satisfies p.
    */

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = (y:Int) => exists(s, x => f(x) == y)

  /**
    * This is similar as how we define a singleton set: For any y, if there exists
    * an element x in s that satisfies the condition f(x) equals y, then y is in new Set map.
    * val t = map(Set(1,2), x => x * 2)
    *println(contains(t, 1))
    *println(contains(t, 2))
    *println(contains(t, 3))
    *println(contains(t, 4))
    */



  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }


}
