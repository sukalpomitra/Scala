package example

/**
  * Created by Sukalpo Mitra on 16/6/2016.
  */
object mapReduceCurrying extends App {

  def mapReduce(f:Int => Int, combine:(Int, Int) => Int, zero:Int)(a:Int, b:Int):Int = {
    if (a > b)
      zero
    else
      combine(f(a), mapReduce(f,combine,zero)(a+1, b))
  }

  def product(f:Int => Int)(a:Int, b:Int):Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  println(product(x => x * x)(3, 4))

  def fact(n: Int):Int = product(x => x)(1, n)

  print(fact(5))

}
