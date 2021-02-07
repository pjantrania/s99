package s99.arithmetic

import s99.S99List.{range, length, encodeDirect}
import scala.language.implicitConversions

class S99Int(val start: Int) {
  import S99Int._

  def isPrime =
    range(2, Math.sqrt(this.start).toInt).forall(n => this.start % n != 0)

  def isCoprimeTo(candidate: Int) = gcd(this.start, candidate) == 1

  def totient = length(range(1, this.start).filter(this.start.isCoprimeTo(_)))

  def primeFactors = {
    def helper(primes: List[Int], target: Int): List[Int] = primes match {
      case Nil                        => List()
      case x :: xs if target % x == 0 => x :: helper(primes, target / x)
      case x :: xs                    => helper(xs, target)
    }

    helper(range(2, this.start / 2).filter(_.isPrime), this.start)
  }

  def primeFactorMultiplicity =
    encodeDirect(this.primeFactors).map(x => (x._2, x._1))

  def improvedTotient = this.primeFactorMultiplicity
    .map(x =>
      x match {
        case (f, m) => (f - 1) * Math.pow(f, m - 1).toInt
      }
    )
    .reduce((a, b) => a * b)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def s99Int2Int(s: S99Int): Int = s.start

  def gcd(a: Int, b: Int): Int = {
    b match {
      case _ if b > a => gcd(b, a)
      case 0          => a
      case _          => gcd(b, a % b)
    }
  }
}
