import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import s99.logic.S99Logic.{not => not2}
import s99.logic.S99Logic._

class S99LogicSpec extends AnyFlatSpec with should.Matchers {
  "get_truth_table" should "return all combos of truth table" in {
    get_truth_table((a: Boolean, b: Boolean) => and(a, b)) should be(
      List(
        (true, true, true),
        (true, false, false),
        (false, true, false),
        (false, false, false)
      )
    )
  }

  "table" should "return formatted truth table" in {
    table((a: Boolean, b: Boolean) => and(a, b)) should be(
      """|A     B     Result
         |true  true  true
         |true  false false
         |false true  false
         |false false false""".stripMargin
    )
  }

  "table" should "support operator based functions" in {
    table((a: Boolean, b: Boolean) => a and (a or not2(b))) should be(
      """|A     B     Result
         |true  true  true
         |true  false true
         |false true  false
         |false false false""".stripMargin
    )
  }

  "grey" should "return list of 2^n strings that follow the grey code" in {
    grey(3) should be(
      List("000", "001", "011", "010", "110", "111", "101", "100")
    )
  }

  "huffman" should "return list of (symbol, code) pairs according to huffman code" in {
    huffman(
      List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5))
    ) should contain theSameElementsAs(
      List(
        ('a', "0"),
        ('b', "101"),
        ('c', "100"),
        ('d', "111"),
        ('e', "1101"),
        ('f', "1100")
      )
    )
  }
}
