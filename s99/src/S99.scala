object S99 {
  def penultimate[A](l: List[A]): Option[A] = l match {
    case x :: y :: Nil => Some(x)
    case x :: Nil      => None
    case x :: xs       => penultimate(xs)
    case _             => None
  }

  def nth[A](k: Int, l: List[A]): Option[A] = k match {
    case 0                        => l.headOption
    case n if n > 0 && !l.isEmpty => nth(n - 1, l.tail)
    case _                        => None
  }

  def length[A](l: List[A]): Int = l match {
    case Nil     => 0
    case x :: xs => 1 + length(xs)
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

  def encodeDirect[A](l: List[A]): List[Tuple2[Int, A]] = {
    def helper(
        l: List[A],
        c: List[Tuple2[Int, A]]
    ): List[Tuple2[Int, A]] = l match {
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

  def decode[A](l: List[Tuple2[Int, A]]): List[A] = {
    def helper(l: List[Tuple2[Int, A]], c: List[A]): List[A] = l match {
      case Nil                 => c
      case x :: xs if x._1 > 0 => helper((x._1 - 1, x._2) :: xs, x._2 :: c)
      case x :: xs             => helper(xs, c)
    }

    l match {
      case Nil     => List()
      case x :: xs => reverse(helper((x._1 - 1, x._2) :: xs, List(x._2)))
    }
  }

  def duplicate[A](l: List[A]): List[A] = l match {
    case Nil     => List()
    case x :: xs => x :: x :: duplicate(xs)
  }

  def duplicateN[A](n: Int, l: List[A]): List[A] = {
    def helper(n: Int, c: Int, l: List[A]): List[A] = l match {
      case Nil              => List()
      case x :: xs if c > 0 => x :: helper(n, c - 1, l)
      case x :: xs          => helper(n, n, xs)
    }

    l match {
      case Nil => List()
      case _   => helper(n, n, l)
    }
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    def helper(n: Int, c: Int, l: List[A]): List[A] = l match {
      case Nil => List()
      case x :: xs if c == 1 => helper(n, n, xs)
      case x :: xs => x :: helper(n, c-1, xs)
    }

    helper(n, n, l)
  }

  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    def helper(n: Int, l: List[A], c: (List[A], List[A])): (List[A], List[A]) = l match {
      case x :: xs if n > 0 => helper(n-1, xs, (x :: c._1, xs))
      case _ => c
    }

    val r = helper(n, l, (List(), List()))
    (reverse(r._1), r._2)
  }
}
