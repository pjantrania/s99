import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.AppendedClues
import s99.arithmetic.S99Int._

class S99IntSpec extends AnyFlatSpec with should.Matchers with AppendedClues {
  "isPrime" should "be true for prime numbers" in {
    List(2, 3, 5, 7, 13, 23, 29, 31, 47, 113).foreach(x =>
      x.isPrime should be(true) withClue s"$x should be prime but isn't"
    )
  }

  "isPrime" should "be false for non-primes" in {
    List(4, 6, 15, 111, 1000).foreach(x =>
      x.isPrime should be(false) withClue s"$x shouldn't be prime but is"
    )
  }

  "gcd" should "return greatest common divisor of the inputs" in {
    gcd(36, 63) should be(9)
  }
}
