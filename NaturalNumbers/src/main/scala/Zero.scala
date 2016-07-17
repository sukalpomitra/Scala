abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this) //always call the successor
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}


/**
  * Created by Sukalpo Mitra on 17/7/2016.
  */
object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("Not a Natural Number")
  def + (that: Nat) = that //0 + that = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("Not a Natural Number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n //predecor is n as succ is n + 1
  def + (that: Nat) = new Succ(n + that) // n+that will give 1 less as n is predecessor so new Succ(n + that)
  def - (that: Nat) = if (that.isZero) n else n - that.predecessor
}
