package s99.binarytree

sealed abstract class S99Tree[+T] {
  def isSymmetric: Boolean = this match {
    case End                  => true
    case Node(_, left, right) => left.isMirrorOf(right)
  }

  def addValue[U >: T <% Ordered[U]](x: U): S99Tree[U] = this match {
    case End => Node(x)
    case Node(value, left, right) if x < value =>
      Node(value, left.addValue(x), right)
    case Node(value, left, right) => Node(value, left, right.addValue(x))
  }

  private def isMirrorOf(t: S99Tree[Any]): Boolean = {
    (this, t) match {
      case (t1: Node[Any], t2: Node[Any]) =>
        t1.left.isMirrorOf(t2.right) && t1.right.isMirrorOf(t2.left)
      case (End, End) => true
      case (_, _)     => false
    }
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

  def fromList[U <% Ordered[U]](l: List[U]): S99Tree[U] =
    l.foldLeft(End.asInstanceOf[S99Tree[U]])((a, b) => a.addValue(b))
}
