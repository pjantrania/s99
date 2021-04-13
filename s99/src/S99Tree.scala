package s99.binarytree

sealed abstract class S99Tree[+T] {

  def valueOption: Option[T] = this match {
    case End                      => None
    case Node(value, left, right) => Some(value)
  }

  def fold[B](z: B)(t: (S99Tree[T], B) => B)(f: (B, B, B) => B): B =
    this match {
      case End => t(End, z)
      case n: Node[T] =>
        f(t(n, z), n.left.fold(t(n, z))(t)(f), n.right.fold(t(n, z))(t)(f))
    }

  def map[B](f: T => B): S99Tree[B] =
    this.fold(S99Tree.empty[B]) {
      case (n: Node[T], _) =>
        Node(f(n.value), End, End)
      case (_, z) => z
    } {
      case (root: Node[B], left: S99Tree[B], right: S99Tree[B]) =>
        Node(root.value, left, right)
      case (root, _, _) => root
    }

  /** this doesn't really work--it discards
    * the structure of the tree returned by `f`
    */
  private def flatMap[B](f: T => S99Tree[B]): S99Tree[B] =
    this.fold(S99Tree.empty[B]) {
      case (n: Node[T], _) => f(n.value)
      case (_, z)          => z
    } {
      case (root: Node[B], left: S99Tree[B], right: S99Tree[B]) =>
        Node(root.value, left, right)
      case (root, _, _) => root
    }

  def isSymmetric: Boolean = this match {
    case End                  => true
    case Node(_, left, right) => left.isMirrorOf(right)
  }

  def depth: Int =
    this.fold(0)((_, _) => 0)((_, left, right) => 1 + math.max(left, right))

  def maxDepthDifference: Int = this.fold(0) {
    case (t: Node[T], i) => math.max(i, math.abs(t.left.depth - t.right.depth))
    case (_, i)          => i
  }((_, l, r) => math.max(l, r))

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

  def nodeCount: Int =
    this.fold[Int](0)((_, _) => 0)((root, left, right) => 1 + left + right)

  def isLeafNode: Boolean = this match {
    case Node(value, left, right) => left == End && right == End
    case End                      => false
  }

  def leafCount: Int =
    this.fold(0)((t, _) => if (t.isLeafNode) 1 else 0)(_ + _ + _)

  def leafList: List[T] = this.fold(List.empty[T])((t, l) =>
    if (t.isLeafNode) List(t.valueOption.get) else Nil
  )((x, l, r) => x ::: l ::: r)

  private def isMirrorOf(t: S99Tree[Any]): Boolean = {
    (this, t) match {
      case (t1: Node[Any], t2: Node[Any]) =>
        t1.left.isMirrorOf(t2.right) && t1.right.isMirrorOf(t2.left)
      case (End, End) => true
      case (_, _)     => false
    }
  }

  def collectNodesAtDepth(d: Int): List[S99Tree[T]] =
    this
      .fold((1, List.empty[S99Tree[T]])) {
        case (t, (c, l)) if c == d =>
          t match {
            case Node(value, _, _) => (c + 1, List(Node(value)))
            case End               => (c + 1, List.fill(math.pow(2, d - c).toInt)(End))
          }
        case (_, (c, l)) => (c + 1, List.empty[S99Tree[T]])
      } { (a, b, c) =>
        (d, a._2 ::: b._2 ::: c._2)
      }
      ._2
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
  def empty[T]: S99Tree[T] = End

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
