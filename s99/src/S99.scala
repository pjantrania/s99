package s99
import java.security.InvalidParameterException
import scala.util.Random
object S99List {
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
      case Nil               => List()
      case x :: xs if c == 1 => helper(n, n, xs)
      case x :: xs           => x :: helper(n, c - 1, xs)
    }

    helper(n, n, l)
  }

  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    def helper(n: Int, l: List[A], c: (List[A], List[A])): (List[A], List[A]) =
      l match {
        case x :: xs if n > 0 => helper(n - 1, xs, (x :: c._1, xs))
        case _                => c
      }

    val r = helper(n, l, (List(), List()))
    (reverse(r._1), r._2)
  }

  def slice[A](s: Int, e: Int, l: List[A]): List[A] = l match {
    case x :: xs if s > 0 => slice(s - 1, e - 1, xs)
    case x :: xs if e > 0 => x :: slice(s, e - 1, xs)
    case _ if s > e       => throw new InvalidParameterException("`s` must be >= `e`")
    case _                => List()
  }

  def rotate[A](n: Int, l: List[A]): List[A] = {
    val (left, right) = split(n, l)
    right :++ left
  }

  def removeAt[A](n: Int, l: List[A]): (List[A], A) = l match {
    case Nil               => (List(), Nil.asInstanceOf[A])
    case x :: xs if n == 0 => (xs, x)
    case x :: xs => {
      val (sublist, element) = removeAt(n - 1, xs)
      (x :: sublist, element)
    }
  }

  def insertAt[A](e: A, n: Int, l: List[A]): List[A] = l match {
    case Nil               => List()
    case x :: xs if n == 0 => e :: x :: xs
    case x :: xs           => x :: insertAt(e, n - 1, xs)
  }

  def range(s: Int, e: Int): List[Int] = s match {
    case n if n <= e => n :: range(n + 1, e)
    case _           => List()
  }

  def randomSelect[A](
      n: Int,
      l: List[A],
      ra: Option[Random] = None
  ): List[A] = {
    val r = ra.getOrElse(Random)
    n match {
      case 0 => List()
      case _ => {
        val (remaining, selected) = removeAt(r.nextInt(length(l)), l)
        selected :: randomSelect(n - 1, remaining, Some(r))
      }
    }
  }

  def lotto(n: Int, m: Int): List[Int] = randomSelect(n, range(1, m))

  def randomPermute[A](l: List[A]): List[A] = randomSelect(length(l), l)

  def combinations[A](n: Int, l: List[A]): List[List[A]] = n match {
    case 1 => l.map(x => List(x))
    case _ =>
      l match {
        case Nil => List()
        case x :: xs =>
          combinations(n - 1, xs).map(y => x :: y) :++ combinations(n, xs)
      }
  }

  // https://stackoverflow.com/questions/19385235/how-to-paramaterize-int-as-ordered-in-scala
  private def sort_impl[A, B <% Ordered[B]](
      l: List[A],
      f: (A) => B
  ): List[A] = {
    def insertOrdered(a: A, l: List[A]): List[A] = l match {
      case Nil                    => List(a)
      case x :: xs if f(a) < f(x) => a :: x :: xs
      case x :: xs                => x :: insertOrdered(a, xs)
    }

    def helper(l: List[A], c: List[A]): List[A] = l match {
      case Nil     => c
      case x :: xs => helper(xs, insertOrdered(x, c))
    }

    helper(l, List())
  }

  def sort[A <% Ordered[A]](l: List[A], f: (A) => A = (x: A) => x): List[A] =
    sort_impl(l, f)

  def lsort[A <% Ordered[A]](l: List[List[A]]): List[List[A]] =
    sort_impl(l, (x: List[A]) => length(x))
}
