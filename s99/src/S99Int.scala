package s99.arithmetic

import s99.S99List.{range}
import scala.language.implicitConversions

class S99Int(val start: Int) {
  import S99Int._
  def isPrime: Boolean =
    range(2, Math.sqrt(this.start).toInt).forall(n => this.start % n != 0)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def s99Int2Int(s: S99Int): Int = s.start
}
