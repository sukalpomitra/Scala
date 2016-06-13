package recfun

import scala.collection.mutable

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    print(balance("(if(zero?x)max(/1x))".toList))
    print(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    print(balance(":-)".toList))
    print(balance("())(".toList))
    println()
    print(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], queue: mutable.Queue[String]): Boolean = {
        if (chars.isEmpty)
          queue.isEmpty
        else {
          if (chars.head == '(')
            queue += "("
          else if (chars.head == ')' && !queue.isEmpty)
            queue.dequeue
          else if (chars.head == ')' && queue.isEmpty)
            queue += "("
          loop(chars.tail, queue)
        }
      }
      loop(chars, mutable.Queue())
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def count(capacity: Int, changes: List[Int]): Int = {
        if(capacity == 0)
          1
        else if(capacity < 0)
          0
        else if(changes.isEmpty && capacity>=1 )
          0
        else
          count(capacity, changes.tail) + count(capacity - changes.head, changes)
      }

      count(money, coins.sortWith(_.compareTo(_) < 0))
    }
  }
