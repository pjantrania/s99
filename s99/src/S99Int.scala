package s99.arithmetic

import s99.S99List.{range, length, encodeDirect}
import scala.language.implicitConversions
import scala.collection.immutable.TreeMap

class S99Int(val start: Int) {
  import S99Int._

  def isPrime = primeStream
    .takeWhile(i => i * i <= this.start)
    .forall(n => this.start % n != 0)

  def isCoprimeTo(candidate: Int) = gcd(this.start, candidate) == 1

  def totient = length(range(1, this.start).filter(this.start.isCoprimeTo(_)))

  def primeFactors = {
    def helper(primes: Iterable[Int], target: Int): List[Int] = primes match {
      case Nil                        => List()
      case x :: xs if target % x == 0 => x :: helper(primes, target / x)
      case x :: xs                    => helper(xs, target)
    }

    helper(listPrimesInRange(2 to this.start / 2), this.start)
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

  def goldbach: Option[(Int, Int)] = {
    if(this.start % 2 != 0)
      return None

    val maxPrime = listPrimesInRange(2 to this.start).last
    return Some((this.start - maxPrime, maxPrime))
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def s99Int2Int(s: S99Int): Int = s.start

  lazy val primeStream: Stream[Int] = 2 #:: Stream
    .from(3)
    .filter(i =>
      primeStream.takeWhile { j => j * j <= i }.forall { k => i % k > 0 }
    )

  def gcd(a: Int, b: Int): Int = {
    b match {
      case _ if b > a => gcd(b, a)
      case 0          => a
      case _          => gcd(b, a % b)
    }
  }

  def listPrimesInRange(r: Range) =
    primeStream.dropWhile(_ < r.start).takeWhile(_ <= r.end).toList
  
  def goldbachList(r: Range) = TreeMap(r.filter(_ % 2 == 0).map(i => i -> i.goldbach.get):_*)
}
