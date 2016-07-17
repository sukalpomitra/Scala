/**
  * Created by Sukalpo Mitra on 17/7/2016.
  */
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr

object Test extends App {

  def eval(e: Expr) : Int = e match {
    case Number(n) => n
    case Sum(l, r) => eval(l) + eval(r)
  }

  def show(e:Expr) : String = e match {
    case Number(n) => n.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }

  println(eval(Sum(Number(1), Number(2))))
  print(show(Sum(Number(1), Number(2))))
}