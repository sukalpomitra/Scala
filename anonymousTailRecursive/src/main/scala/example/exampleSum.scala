package example

/**
  * Created by Sukalpo Mitra on 15/6/2016.
  */
object exampleSum extends App {
  def sum(f:Int => Int, a:Int, b:Int) : Int = {
    def loop(a:Int, acc:Int):Int = {
      if (a > b) acc
      else
        loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  def sum1(f:(Int,=> Int) => Int, a:Int, b:Int) : Int = {
    def loop(a:Int, acc:Int):Int = {
      if (a > b) acc
      else
        loop(a + 1, f(a,a) + acc)
    }
    loop(a, 0)
  }

  println(sum(x => x*x, 3, 5))
  print(sum1((x,y) => x*y, 3, 5))
}
