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
}
