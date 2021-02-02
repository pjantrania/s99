object S99 {
  def findLastButOne[A](l: List[A]): Option[A] = l match {
    case x :: y :: Nil => Some(x)
    case x :: Nil      => None
    case x :: xs       => findLastButOne(xs)
    case _             => None
  }

  def findKth[A](k: Int, l: List[A]): Option[A] = k match {
    case 0                        => l.headOption
    case n if n > 0 && !l.isEmpty => findKth(n - 1, l.tail)
    case _                        => None
  }

  def getLength[A](l: List[A]): Int = l match {
    case Nil     => 0
    case x :: xs => 1 + getLength(xs)
  }

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil     => l
    case x :: xs => reverse(xs) :+ x
  }

  def isPalindrome[A](l: List[A]) = l == reverse(l)

  def flatten(l: List[Any]): List[Int] = l match {
    case Nil => List()
    case x :: xs =>
      x match {
        case n: Int       => n :: flatten(xs)
        case s: List[Any] => flatten(s) :++ flatten(xs)
      }
  }

  def compress(l: List[Char]): List[Char] = l match {
    case Nil                    => List()
    case x :: Nil               => l
    case x :: y :: xs if x == y => compress(y :: xs)
    case x :: xs                => x :: compress(xs)
  }

  def encode(l: List[Char]): List[Tuple2[Int, Char]] = {
    def helper(
        l: List[Char],
        c: List[Tuple2[Int, Char]]
    ): List[Tuple2[Int, Char]] = l match {
      case Nil => c
      case x :: xs if x == c.head._2 =>
        helper(xs, (c.head._1 + 1, x) :: c.tail)
      case x :: xs => helper(xs, (1, x) :: c)
    }

    l match {
      case Nil     => List()
      case x :: xs => reverse(helper(xs, List((1, x))))
    }
  }
}
