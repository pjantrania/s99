import org.scalatest._
import flatspec._
import matchers._
import java.security.InvalidParameterException

class S99Spec extends AnyFlatSpec with should.Matchers {

  "nth" should "return list element at index k" in {
    S99.nth(1, List(1, 2, 3)) should be(Some(2))
  }

  "nth" should "return None if index is out of bounds" in {
    S99.nth(10, List(1, 2, 3)) should be(None)
  }

  "isPalindrome" should "return true if list is palindrome" in {
    List(
      List(1, 2, 1),
      List(1, 2, 2, 1),
      List(),
      List(1234567890),
      List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    ).foreach(S99.isPalindrome(_) should be(true))
  }

  "isPalindrome" should "return false if list is not palindrome" in {
    List(
      List(1, 2, 3),
      List(1, 2, 1, 1)
    ).foreach(S99.isPalindrome(_) should be(false))
  }

  "flatten" should "do nothing to a flat list" in {
    S99.flatten(List(1, 2, 3)) should be(List(1, 2, 3))
  }

  "flatten" should "flatten a single nested list" in {
    S99.flatten(List(List(1, 2, 3))) should be(List(1, 2, 3))
  }

  "flatten" should "flatten a mixed list of flat lists and Ints" in {
    S99.flatten(List(1, 2, List(1, 2, 3), 4)) should be(List(1, 2, 1, 2, 3, 4))
  }

  "flatten" should "flatten a mixed list of nested list and Ints" in {
    S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(
      List(1, 1, 2, 3, 5, 8)
    )
  }

  "compress" should "remove duplicate characters from list" in {
    S99.compress(List('a', 'a', 'b', 'c')) should be(List('a', 'b', 'c'))
    S99.compress(List('a', 'a', 'a', 'b', 'c', 'e', 'e', 'e')) should be(
      List('a', 'b', 'c', 'e')
    )
  }

  "encodeDirect" should "run-length encodeDirect the list" in {
    S99.encodeDirect(List('a', 'a', 'b', 'c')) should be(
      List((2, 'a'), (1, 'b'), (1, 'c'))
    )

    S99.encodeDirect(
      List('a', 'a', 'a', 'b', 'b', 'c', 'e', 'e', 'e')
    ) should be(
      List((3, 'a'), (2, 'b'), (1, 'c'), (3, 'e'))
    )
  }

  "decode" should "decode run length encoded list" in {
    S99.decode(
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    ) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "decode" should "round trip a list" in {
    val l = List('a', 'a', 'a', 'b', 'c', 'c', 'd', 'e', 'e')
    S99.decode(S99.encodeDirect(l)) should be(l)

  }

  "duplicate" should "duplicate each element in the list" in {
    S99.duplicate(List('a, 'b, 'c, 'c, 'd)) should be(
      List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    )
  }

  "duplicateN" should "duplicate each element in the list N times" in {
    S99.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    )
  }

  "drop" should "drop every Nth item in the list" in {
    S99.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  "split" should "split a list in 2 lists, the first of which has length N" in {
    S99.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(
      (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }

  "slice" should "return sublist in index range [s, e)" in {
    S99.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(
      List('d, 'e, 'f, 'g)
    )
  }

  "slice" should "return empty list when s == e" in {
    S99.slice(2, 2, List(1, 2, 3)) should be(List())
  }

  an[InvalidParameterException] should be thrownBy S99.slice(2, 1, List('a))

  "rotate" should "move first N elements to the end of the list" in {
    S99.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    )
  }

  "removeAt" should "return list without element at index N" in {
    S99.removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd),'b))
  }
}
