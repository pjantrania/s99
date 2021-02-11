package s99.logic
import s99.S99List.{flatten}
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.PriorityQueue

class S99Logic(a: Boolean) {
  import S99Logic._
  def and(b: Boolean): Boolean = a && b
  def or(b: Boolean): Boolean = a || b
  def nand(b: Boolean): Boolean = not(a.and(b))
  def nor(b: Boolean): Boolean = not(a.or(b))
  def xor(b: Boolean): Boolean = a.nand(b).and(a.or(b))
  def impl(b: Boolean): Boolean = b.or(not(a))
  def equ(b: Boolean): Boolean = b.impl(a).and(a.impl(b))
}

object S99Logic {
  implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)
  def and(a: Boolean, b: Boolean): Boolean = a && b
  def or(a: Boolean, b: Boolean): Boolean = a || b
  def not(a: Boolean): Boolean = !a
  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: Boolean): Boolean = and(or(a, b), nand(a, b))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  def equ(a: Boolean, b: Boolean): Boolean = and(impl(a, b), impl(b, a))

  def get_truth_table(
      f: (Boolean, Boolean) => Boolean
  ): Iterable[(Boolean, Boolean, Boolean)] =
    List(true, false)
      .map(a => List(true, false).map(b => (a, b, f(a, b))))
      .flatten

  def table(f: (Boolean, Boolean) => Boolean): String = {
    val header = "A".padTo(6, ' ') + "B".padTo(6, ' ') + "Result"
    (
      header :: get_truth_table(f).map { case (a, b, result) =>
        a.toString().padTo(6, ' ') +
          b.toString().padTo(6, ' ') +
          result.toString()
      }.toList
    ).mkString("\n")
  }

  def huffman[A](symbolFrequencies: List[(A, Int)]): List[(A, String)] = {
    def helper(root: HuffmanNode): List[(A, String)] = root match {
      case LeafNode(letter, frequency) => List((letter.asInstanceOf[A], ""))
      case InternalNode(value, left, right) =>
        helper(left).map { case (symbol, code) =>
          (symbol, "0" + code)
        } :++ helper(right).map { case (symbol, code) =>
          (symbol, "1" + code)
        }
    }

    helper(buildHuffmanTree(symbolFrequencies))
  }

  private def buildHuffmanTree[A](
      symbolFrequencies: List[(A, Int)]
  ): HuffmanNode = {
    val pq = PriorityQueue[HuffmanNode](
      symbolFrequencies.map { case (symbol, freq) =>
        LeafNode(symbol, freq)
      }: _*
    )

    while (pq.length > 1) {
      val (min1, min2) = (pq.dequeue(), pq.dequeue())
      pq.enqueue(InternalNode(min1.value + min2.value, min1, min2))
    }

    return pq.dequeue()
  }

  lazy val grey = memoize(greyCodeImpl)

  private def greyCodeImpl(n: Int): List[String] = n match {
    case 1 => List("0", "1")
    case _ =>
      greyCodeImpl(1)
        .map(a =>
          a(0) match {
            case '0' => greyCodeImpl(n - 1).map(b => a + b)
            case '1' => greyCodeImpl(n - 1).reverse.map(b => a + b)
          }
        )
        .flatten
  }

  private def memoize[K, V](f: K => V): K => V = {
    val cache = MutableMap.empty[K, V]
    k =>
      cache.getOrElse(
        k, {
          cache.update(k, f(k))
          cache(k)
        }
      )
  }

  private trait HuffmanNode extends Ordered[HuffmanNode] {
    val value: Int

    def compare(that: HuffmanNode): Int = -this.value compare -that.value
  }

  private case class InternalNode(
      val value: Int,
      val left: HuffmanNode = Nil.asInstanceOf[HuffmanNode],
      val right: HuffmanNode = Nil.asInstanceOf[HuffmanNode]
  ) extends HuffmanNode

  private case class LeafNode[A](val letter: A, val frequency: Int)
      extends HuffmanNode {
    override val value: Int = frequency
  }
}
