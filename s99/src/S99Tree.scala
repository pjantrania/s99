package s99.binarytree

sealed abstract class S99Tree[+T]
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
    case 1 => List(Node(x, End, End))
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
        case 0 => cBalanced(n / 2, x).map(t => Node(x, t, t))
      }
  }
}
