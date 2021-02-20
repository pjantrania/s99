package s99.binarytree

sealed abstract class S99Tree[+T] {
  def isSymmetric: Boolean = this match {
    case End                  => true
    case Node(_, left, right) => left.isMirrorOf(right)
  }

  def depth: Int = this match {
    case End                      => 0
    case Node(value, left, right) => 1 + math.max(left.depth, right.depth)
  }

  def maxDepthDifference: Int = this match {
    case End => 0
    case Node(value, left, right) =>
      List(
        math.abs(left.depth - right.depth),
        left.maxDepthDifference,
        right.maxDepthDifference
      ).max
  }

  def addValue[U >: T <% Ordered[U]](x: U): S99Tree[U] = this match {
    case End => Node(x)
    case Node(value, left, right) if x < value =>
      Node(value, left.addValue(x), right)
    case Node(value, left, right) => Node(value, left, right.addValue(x))
  }

  def stringRepr(): String = {
    (1 to this.depth)
      .map(i => {
        val prefixSpaceCount = math.pow(2, this.depth - i).toInt - 1
        val separatorSpaceCount = math.pow(2, this.depth - i + 1).toInt - 1
        " ".repeat(prefixSpaceCount) + this
          .collectNodesAtDepth(i)
          .map {
            case End                      => "."
            case Node(value, left, right) => value.toString()
          }
          .mkString(" ".repeat(separatorSpaceCount))
      })
      .mkString("\n")
  }

  def nodeCount: Int = this match {
    case Node(value, left, right) => 1 + left.nodeCount + right.nodeCount
    case End                      => 0
  }

  private def isMirrorOf(t: S99Tree[Any]): Boolean = {
    (this, t) match {
      case (t1: Node[Any], t2: Node[Any]) =>
        t1.left.isMirrorOf(t2.right) && t1.right.isMirrorOf(t2.left)
      case (End, End) => true
      case (_, _)     => false
    }
  }

  private def collectNodesAtDepth(d: Int, c: Int = 1): List[S99Tree[Any]] =
    this match {
      case End                         => List.fill(math.pow(2, d - c).toInt)(End)
      case Node(value, _, _) if c == d => List(Node(value))
      case Node(_, left, right) =>
        left.collectNodesAtDepth(d, c + 1) :++ right.collectNodesAtDepth(
          d,
          c + 1
        )
    }
}

case class Node[+T](value: T, left: S99Tree[T], right: S99Tree[T])
    extends S99Tree[T] {
  override def toString =
    "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends S99Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object S99Tree {
  def cBalanced[A](n: Int, x: A): List[S99Tree[A]] = n match {
    case 0 => List(End)
    case 1 => List(Node(x))
    case _ =>
      (n - 1) % 2 match {
        case 1 =>
          cBalanced((n - 1) / 2, x)
            .map(t =>
              cBalanced((n - 1) / 2 + 1, x)
                .map(s => List(Node(x, t, s), Node(x, s, t)))
                .flatten
            )
            .flatten
        case 0 =>
          cBalanced(n / 2, x).flatMap(s =>
            cBalanced(n / 2, x).map(t => Node(x, s, t))
          )
      }
  }

  def symmetricBalancedTrees[A](n: Int, x: A): List[S99Tree[A]] =
    (n % 2) match {
      case 1 => cBalanced(n, x).filter(_.isSymmetric)
      case 0 => List.empty[S99Tree[A]]
    }

  def fromList[U <% Ordered[U]](l: IterableOnce[U]): S99Tree[U] =
    l.foldLeft(End.asInstanceOf[S99Tree[U]])((a, b) => a.addValue(b))

  def hbalTrees[A](h: Int, x: A): List[S99Tree[A]] = h match {
    case 0 => List(End)
    case 1 => List(Node(x))
    case _ => {
      val biggerSubtrees = hbalTrees(h - 1, x)
      val smallerSubtrees = hbalTrees(h - 2, x)

      biggerSubtrees.flatMap(t => {
        val oneBiggerOneSmaller =
          smallerSubtrees.flatMap(s => List(Node(x, t, s), Node(x, s, t)))
        val bothSameHeight = biggerSubtrees.map(v => Node(x, v, t))

        oneBiggerOneSmaller :++ bothSameHeight
      })
    }
  }

  def minHbalNodes(h: Int): Int = h match {
    case 0 => 0
    case 1 => 1
    case _ => 1 + minHbalNodes(h - 1) + minHbalNodes(h - 2)
  }

  def maxHbalNodes(h: Int): Int = math.pow(2, h).toInt - 1

  def minHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case _ => minHbalHeight(n / 2) + 1
  }

  def maxHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => Stream.from(2).dropWhile(x => minHbalNodes(x + 1) <= n).head
  }

  def hbalTreesWithNodes[A](n: Int, x: A): List[S99Tree[A]] =
    (minHbalHeight(n) to maxHbalHeight(n))
      .flatMap(hbalTrees(_, x))
      .filter(_.nodeCount == n)
      .toList

}
