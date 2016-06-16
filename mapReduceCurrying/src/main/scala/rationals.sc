val x = new Rational(1,3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numer
x.denom



x.add(y)

x.sub(y).sub(z)

y.add(y)

x.less(y)
x.max(y)

new Rational(1,0)

class Rational(x:Int, y:Int) {
  require(y > 0, "The denominator cannot be zero")
  private def gcd(a:Int, b:Int):Int = if (b == 0) a
  else gcd(b, a % b)

  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def this(x:Int) = this(x, 1)

  def less(that: Rational) = numer * that.denom <
    that.numer * denom

  def max(that: Rational) = if (this.less(that)) that
    else this

  override def toString = numer + "/" + denom

  def add(that: Rational): Rational =
    new Rational((numer * that.denom) + (that.numer * denom),
      denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)
}